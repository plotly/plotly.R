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
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  func <- shiny::exprToFunction(expr, env, quoted)
  renderFunc <- function(shinysession, name, ...) {
    # The line below is the reason why we can't just use htmlwidgets::shinyRenderWidget()
    # shinyRenderWidget() assumes the expression returns an htmlwidget object.
    # But plotly objects are not htmlwidgets objects, so we need to convert
    instance <- toWidget(func())
    if (!is.null(instance$elementId)) {
      warning("Ignoring explicitly provided widget ID \"", 
              instance$elementId, "\"; Shiny doesn't use them")
    }
    deps <- .subset2(instance, "dependencies")
    deps <- lapply(htmltools::resolveDependencies(deps), 
                   shiny::createWebDependency)
    payload <- c(htmlwidgets:::createPayload(instance), list(deps = deps))
    htmlwidgets:::toJSON(payload)
  }
  shiny::markRenderFunction(plotly::plotlyOutput, renderFunc)
}
