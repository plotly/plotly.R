#' Shiny bindings for plotly
#' 
#' Output and render functions for using plotly within Shiny 
#' applications and interactive Rmd documents.
#' 
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param inline use an inline (\code{span()}) or block container 
#' (\code{div()}) for the output
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
plotlyOutput <- function(outputId, width = "100%", height = "400px", 
                         inline = FALSE) {
  htmlwidgets::shinyWidgetOutput(
    outputId = outputId, 
    name = "plotly", 
    width = width, 
    height = height, 
    inline = inline, 
    package = "plotly"
  )
}

#' @rdname plotly-shiny
#' @export
renderPlotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  # https://github.com/ramnathv/htmlwidgets/issues/166#issuecomment-153000306
  expr <- call("as.widget", expr)
  shinyRenderWidget(expr, plotlyOutput, env, quoted = TRUE)
}
