#' Shiny bindings for plotly
#' 
#' Output and render functions for using plotly within Shiny 
#' applications and interactive Rmd documents.
#' 
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   `"400px"`, `"auto"`) or a number, which will be coerced to a
#'   string and have `"px"` appended.
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
  session <- getDefaultReactiveDomain()
  eventIDs <- paste(p$x$shinyEvents, p$x$source, sep = "-")
  inputIDs <- paste(p$x$shinyInputs, p$x$source, sep = "-")
  session$userData$plotlyShinyEventIDs <- unique(c(
    session$userData$plotlyShinyEventIDs,
    eventIDs
  ))
  session$userData$plotlyShinyInputIDs <- unique(c(
    session$userData$plotlyShinyInputIDs,
    inputIDs
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
#' @export
#' @seealso [event_register], [event_unregister]
#' @references 
#'   * <https://plotly-book.cpsievert.me/shiny-plotly-inputs.html> 
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
  # TODO: resolve priority
  priority <- match.arg(priority)
  # check to see if this event-source-priority combo is registered
  eventID <- paste(event, source, sep = "-")
  keyName <- if (priority == "event") "plotlyShinyEventIDs" else "plotlyShinyInputIDs"
  if (!eventID %in% session$userData[[keyName]]) {
    stop(
      "The '", event, "' event has not been registered for a source ID ",
      "of '", source, "' with priority '", priority, "'. ", 
      "Please add `event_register(p, '", event, "', '", priority, "')` to your plot `p`."
    )
  }
  
  inputName <- sprintf("%s-%s-%s", event, source, priority)
  val <- session$rootScope()$input[[inputName]]
  
  # legend clicking returns trace(s), which shouldn't be simplified...
  parseJSON <- if (event %in% c("plotly_legendclick", "plotly_legenddoubleclick")) {
    from_JSON
  } else {
    function(x) jsonlite::parse_json(x, simplifyVector = TRUE)
  }
  
  if (is.null(val)) val else parseJSON(val)
}


#' Register a shiny input value 
#' 
#' @inheritParams event_data
#' @seealso [event_data]
#' @export
#' @author Carson Sievert
event_register <- function(p, event = NULL, priority = c("input", "event")) {
  priority <- match.arg(priority)
  event <- match.arg(event, event_data_events())
  if (priority == "event") shiny_event_add(p, event) else shiny_input_add(p, event)
}

#' Un-register a shiny input value
#' 
#' @inheritParams event_data
#' @seealso [event_data]
#' @export
#' @author Carson Sievert
event_unregister <- function(p, event = NULL, priority = c("input", "event")) {
  priority <- match.arg(priority)
  event <- match.arg(event, event_data_events())
  if (priority == "event") shiny_event_remove(p, event) else shiny_input_remove(p, event)
}


# helpers
shiny_event_add <- function(p, event) {
  p <- shiny_defaults_set(p)
  p$x$shinyEvents <- unique(c(p$x$shinyEvents, event))
  p
}

shiny_input_add <- function(p, event) {
  p <- shiny_defaults_set(p)
  p$x$shinyInputs <- unique(c(p$x$shinyInputs, event))
  p
}

shiny_event_remove <- function(p, event) {
  p <- shiny_defaults_set(p)
  p$x$shinyEvents <- setdiff(p$x$shinyEvents, event)
  p
}

shiny_input_remove <- function(p, event) {
  p <- shiny_defaults_set(p)
  p$x$shinyInputs <- setdiff(p$x$shinyInputs, event)
  p
}

shiny_defaults_set <- function(p) {
  p$x$shinyEvents <- p$x$shinyEvents %||% shiny_event_defaults()
  p$x$shinyInputs <- p$x$shinyInputs %||% shiny_input_defaults()
  p
}

shiny_input_defaults <- function() {
  c(
    "plotly_hover", 
    "plotly_click", 
    "plotly_selected", 
    "plotly_relayout", 
    "plotly_brushed",
    "plotly_brushing",
    "plotly_clickannotation"
  )
}

shiny_event_defaults <- function() {
  c(
    "plotly_doubleclick", 
    "plotly_deselect", 
    "plotly_afterplot"
  )
}

event_data_events <- function() {
  eval(formals(event_data)$event)
}
