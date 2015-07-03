#' Shiny ui output function
#' @param outputId output variable to read the plot from
#' @param width width of the output.
#' @param height height of the output.
#' @seealso http://shiny.rstudio.com/articles/building-outputs.html
#' @export
#' 
plotlyOutput <- function(outputId, width = "100%", height = "550px") {
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  deps <- lapply(plotly_dependencies(), shiny::createWebDependency)
  htmltools::attachDependencies(
    # TODO: allow users to specify their own src location?
    htmltools::tags$iframe(id = outputId, src = "https://plot.ly/~playground/7.embed",
                           class = "graphs", style = "border:none;", seamless = TRUE, 
                           width = width, height = height),
    deps
  )
}

#' Render a plotly graph in shiny
#' 
#' Shiny server output function customized for plotly.
#' 
#' @param expr An expression that creates a ggplot or plotly object
#' @param envir The environment in which to evaluate \code{expr}.
#' @param quoted Is expr a quoted expression (with \code{quote()})? 
#' This is useful if you want to save an expression in a variable.
#' @export
#' 

renderPlotly <- function(expr, envir = parent.frame(), quoted = FALSE) {
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  func <- shiny::exprToFunction(expr, envir, quoted)
  renderFunc <- function(shinysession, name, ...) {
    p <- func()
    l <- if (is.ggplot(p)) {
      gg2list(p) 
    } else if (is.plotly(p)) {
      eval_plot(get_plot(p))
    } else if (is.list(p)) {
      p
    } else stop("Input to renderPlotly() must be either a ggplot object, a plotly object, or a list.")
    
    # eventually let users alter these?
    l$id <- "trendPlot"
    l$task <- "newPlot"
    # return a list of named lists that describe valid postMessage 
    # commands to be sent to the embedded iframe. See binding.renderValue for 
    # the receiving JS side of this function and https://github.com/plotly/Embed-API 
    # for more about the postMessage graph messages
    list(l)
  }
  # this will tell knitr how to manage the app in an interactive document
  # implementation is similar to htmlwidgets::shinyRenderWidget()
  shiny::markRenderFunction(plotly::plotlyOutput, renderFunc)
}

# html dependencies according htmltools protocols
# these are here basically so we can take advantage of shiny::createWebDependency
plotly_dependencies <- function() {
  list(plotlyEmbed())
}

plotlyEmbed <- function() {
  htmltools::htmlDependency(name = "plotlyEmbed",
                            version = packageVersion("plotly"),
                            src = system.file("shiny", package = "plotly"),
                            script = "plotlyEmbed.js")
}
