#' Shiny bindings for plotly
#' 
#' Output and render functions for using plotly within Shiny 
#' applications and interactive Rmd documents.
#' 
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   `"400px"`, `"auto"`) or a number, which will be coerced to a
#'   string and have `"px"` appended. Note that, for height, using "auto" 
#'   or "100%" generally will not work as expected, because of how 
#'   height is computed with HTML/CSS.
#' @param inline use an inline (`span()`) or block container 
#' (`div()`) for the output
#' @param expr An expression that generates a plotly
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with `quote()`)? This 
#'   is useful if you want to save an expression in a variable.
#'   
#' @importFrom htmlwidgets shinyWidgetOutput
#' @importFrom htmlwidgets shinyRenderWidget
#' @name plotly-shiny
#'
#' @export
plotlyOutput <- function(outputId, width = "100%", height = "400px", 
                         inline = FALSE) {
  htmlwidgets::shinyWidgetOutput(
    outputId = outputId, 
    name = "plotly", 
    width = width, 
    height = height, 
    inline = inline, 
    package = "plotly",
    reportSize = TRUE
  )
}

#' @rdname plotly-shiny
#' @export
renderPlotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  # this makes it possible to pass a ggplot2 object to renderPlotly()
  # https://github.com/ramnathv/htmlwidgets/issues/166#issuecomment-153000306
  expr <- as.call(list(call(":::", quote("plotly"), quote("prepareWidget")), expr))
  renderFunc <- shinyRenderWidget(expr, plotlyOutput, env, quoted = TRUE)
  # remove 'internal' plotly attributes that are known to cause false
  # positive test results in shinytest (snapshotPreprocessOutput was added 
  # in shiny 1.0.3.9002, but we require >= 1.1)
  shiny::snapshotPreprocessOutput(
    renderFunc,
    function(value) {
      json <- from_JSON(value)
      json$x <- json$x[setdiff(names(json$x), c("visdat", "cur_data", "attrs"))]
      to_JSON(json)
    }
  )
}

# Converts a plot, OR a promise of a plot, to plotly
prepareWidget <- function(x) {
  p <- if (promises::is.promising(x)) {
    promises::then(x, plotly_build)
  } else {
    plotly_build(x)
  }
  register_plot_events(p)
  p
}

register_plot_events <- function(p) {
  session <- shiny::getDefaultReactiveDomain()
  eventIDs <- paste(p$x$shinyEvents, p$x$source, sep = "-")
  session$userData$plotlyShinyEventIDs <- unique(c(
    session$userData$plotlyShinyEventIDs,
    eventIDs
  ))
}


#' Access plotly user input event data in shiny
#' 
#' This function must be called within a reactive shiny context.
#' 
#' @param event The type of plotly event. All supported events are listed in the 
#' function signature above (i.e., the usage section).
#' @param source a character string of length 1. Match the value of this string 
#' with the `source` argument in [plot_ly()] (or [ggplotly()]) to respond to  
#' events emitted from that specific plot.
#' @param session a shiny session object (the default should almost always be used).
#' @param priority the priority of the corresponding shiny input value. 
#' If equal to `"event"`, then [event_data()] always triggers re-execution, 
#' instead of re-executing only when the relevant shiny input value changes 
#' (the default).
#' @export
#' @seealso [event_register], [event_unregister]
#' @references 
#'   * <https://plotly-r.com/linking-views-with-shiny.html#shiny-plotly-inputs> 
#'   * <https://plot.ly/javascript/plotlyjs-function-reference/>
#' @author Carson Sievert
#' @examples \dontrun{
#' plotly_example("shiny", "event_data")
#' }

event_data <- function(
  event = c(
    "plotly_hover", "plotly_unhover", "plotly_click", "plotly_doubleclick",
    "plotly_selected", "plotly_selecting", "plotly_brushed", "plotly_brushing", 
    "plotly_deselect", "plotly_relayout", "plotly_restyle", "plotly_legendclick", 
    "plotly_legenddoubleclick", "plotly_clickannotation", "plotly_afterplot"
  ),
  source = "A",
  session = shiny::getDefaultReactiveDomain(),
  priority = c("input", "event")
) {
  if (is.null(session)) {
    stop("No reactive domain detected. This function can only be called \n",
         "from within a reactive shiny context.")
  }
  
  event <- match.arg(event)
  eventID <- paste(event, source, sep = "-")
  
  # It's possible for event_data() to execute before any 
  # relevant input values have been registered (i.e, before 
  # relevant plotly graphs have been executed). Therefore, 
  # we delay checking that a relevant input value has been 
  # registered until shiny flushes
  session$onFlushed(
    function() {
      eventIDRegistered <- eventID %in% session$userData$plotlyShinyEventIDs
      if (!eventIDRegistered) {
        warning(
          "The '", event, "' event tied a source ID of '", source, "' ",
          "is not registered. In order to obtain this event data, ", 
          "please add `event_register(p, '", event, "')` to the plot (`p`) ",
          "that you wish to obtain event data from.",
          call. = FALSE
        )
      }
    }
  )
  
  # legend clicking returns trace(s), which shouldn't be simplified...
  parseJSON <- if (event %in% c("plotly_legendclick", "plotly_legenddoubleclick")) {
    from_JSON
  } else {
    function(x) jsonlite::parse_json(x, simplifyVector = TRUE)
  }
  
  # Handle NULL sensibly
  parseJSONVal <- function(x) {
    if (is.null(x)) x else parseJSON(x)
  }
  
  parsedInputValue <- function() {
    parseJSONVal(session$rootScope()$input[[eventID]])
  }
  
  # events that don't emit any data should _always_ be treated with event priority
  priority <- if (event %in% c("plotly_doubleclick", "plotly_deselect", "plotly_afterplot")) {
    "event"
  } else {
    match.arg(priority)
  }
  
  if (priority == "event") {
    # Shiny.setInputValue() is always called with event priority
    # so simply return the parse input value
    return(parsedInputValue())
    
  } else {
    
    eventHasStorage <- eventID %in% names(session$userData$plotlyInputStore)
    
    if (!eventHasStorage) {
      # store input value as a reactive value to leverage caching
      session$userData$plotlyInputStore <- session$userData$plotlyInputStore %||% shiny::reactiveValues()
      session$userData$plotlyInputStore[[eventID]] <- shiny::isolate(parsedInputValue())
      shiny::observe({
        session$userData$plotlyInputStore[[eventID]] <- parsedInputValue()
      }, priority = 10000, domain = session)
    } 
    
    session$userData$plotlyInputStore[[eventID]]
  }
  
}


#' Register a shiny input value 
#' 
#' @inheritParams event_data
#' @param p a plotly object.
#' @seealso [event_data]
#' @export
#' @author Carson Sievert
event_register <- function(p, event = NULL) {
  event <- match.arg(event, event_data_events())
  shiny_event_add(p, event)
}

#' Un-register a shiny input value
#' 
#' @inheritParams event_data
#' @param p a plotly object.
#' @seealso [event_data]
#' @export
#' @author Carson Sievert
event_unregister <- function(p, event = NULL) {
  event <- match.arg(event, event_data_events())
  shiny_event_remove(p, event) 
}


# helpers
shiny_event_add <- function(p, event) {
  p <- shiny_defaults_set(p)
  p$x$shinyEvents <- unique(c(p$x$shinyEvents, event))
  p
}

shiny_event_remove <- function(p, event) {
  p <- shiny_defaults_set(p)
  p$x$shinyEvents <- setdiff(p$x$shinyEvents, event)
  p
}

shiny_defaults_set <- function(p) {
  p$x$shinyEvents <- p$x$shinyEvents %||% shiny_event_defaults()
  p
}

shiny_event_defaults <- function() {
  c(
    "plotly_hover", 
    "plotly_click", 
    "plotly_selected", 
    "plotly_relayout", 
    "plotly_brushed",
    "plotly_brushing",
    "plotly_clickannotation",
    "plotly_doubleclick", 
    "plotly_deselect", 
    "plotly_afterplot"
  )
}

event_data_events <- function() {
  eval(formals(event_data)$event)
}
