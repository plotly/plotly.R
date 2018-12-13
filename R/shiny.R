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
      json <- from_JSON_safe(value)
      json$x <- json$x[setdiff(names(json$x), c("visdat", "cur_data", "attrs"))]
      to_JSON(json)
    }
  )
}

# Converts a plot, OR a promise of a plot, to plotly
prepareWidget <- function(x) {
  if (promises::is.promising(x)) {
    promises::then(x, ggplotly)
  } else {
    ggplotly(x)
  }
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
  session = shiny::getDefaultReactiveDomain()
) {
  if (is.null(session)) {
    stop("No reactive domain detected. This function can only be called \n",
         "from within a reactive shiny context.")
  }
  
  # make sure the input event is sensible
  event <- match.arg(event)
  src <- sprintf(".clientValue-%s-%s", event, source)
  val <- session$rootScope()$input[[src]]
  
  # legend clicking returns trace(s), which shouldn't be simplified...
  fromJSONfunc <- if (event %in% c("plotly_legendclick", "plotly_legenddoubleclick")) from_JSON else jsonlite::fromJSON
  
  if (is.null(val)) val else fromJSONfunc(val)
}
