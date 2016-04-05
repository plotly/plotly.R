#' Shiny bindings for plotly
#' 
#' Output and render functions for using plotly within Shiny 
#' applications and interactive Rmd documents.
#' 
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param expr An expression that generates a plotly
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This 
#'   is useful if you want to save an expression in a variable.
#'   
#' @importFrom htmlwidgets shinyWidgetOutput
#' @importFrom htmlwidgets shinyRenderWidget
#' @name plotly-shiny
#'
#' @export
plotlyOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "plotly", width, height, package = "plotly")
}

#' @rdname plotly-shiny
#' @export
renderPlotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  # https://github.com/ramnathv/htmlwidgets/issues/166#issuecomment-153000306
  expr <- call("as.widget", expr)
  shinyRenderWidget(expr, plotlyOutput, env, quoted = TRUE)
}


#' Access plotly user input event data in shiny
#' 
#' This function must be called within a reactive shiny context.
#' 
#' @param event The type of plotly event. Currently 'plotly_hover',
#' 'plotly_click', 'plotly_selected', and 'plotly_relayout' are supported.
#' @param source Which plot should the listener be tied to? This 
#' (character string) should match the value of \code{source} in \link{plot_ly}.
#' @export
#' @author Carson Sievert
#' @examples \dontrun{
#' shiny::runApp(system.file("examples", "events", package = "plotly"))
#' }

event_data <- function(event = c("plotly_hover", "plotly_click", "plotly_selected", 
                                 "plotly_relayout"), source = "A") {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    stop("No reactive domain detected. This function can only be called \n",
         "from within a reactive shiny context.")
  }
  val <- session$input[[sprintf(".clientValue-%s-%s", event[1], source)]]
  if (is.null(val)) val else jsonlite::fromJSON(val)
}
