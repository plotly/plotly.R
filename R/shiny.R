#' Shiny ui output function
#' @param outputId output variable to read the plot from
#' @param width width of the output.
#' @param height height of the output.
#' @param offline Use plotly offline, if available? 
#' @seealso http://shiny.rstudio.com/articles/building-outputs.html
#' @export
#' 
plotlyOutput <- function(outputId, width = "100%", height = "550px", offline = TRUE) {
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  if (offline && has_offline()) {
    deps <- lapply(plotly_offline(), shiny::createWebDependency)
    el <- htmltools::tags$div(
      id = outputId,
      class = 'plotly_offline',
      style = sprintf("width: %s; height: %s;", width, height)
    )
  } else {
    deps <- shiny::createWebDependency(plotly_embed())
    el <- htmltools::tags$iframe(
      id = outputId, 
      src = "https://plot.ly/~playground/7.embed",
      class = "plotly_embed", 
      style = "border:none;", 
      seamless = TRUE, 
      width = width, 
      height = height
    )
  }
  htmltools::attachDependencies(el, deps)
}

#' Render a plotly graph in shiny
#' 
#' Shiny server output function customized for plotly.
#' 
#' @param expr An expression that creates a ggplot or plotly object
#' @param envir The environment in which to evaluate \code{expr}.
#' @param quoted Is expr a quoted expression (with \code{quote()})? 
#' @param offline Use plotly offline, if available? 
#' This is useful if you want to save an expression in a variable.
#' @export
#' 

renderPlotly <- function(expr, envir = parent.frame(), quoted = FALSE, offline = TRUE) {
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  func <- shiny::exprToFunction(expr, envir, quoted)
  offline <- offline && has_offline()
  renderFunc <- function(shinysession, name, ...) {
    p <- func()
    l <- if (offline) offline(p) else plotly_build(p)
    # why do I need this????????????
    l$id <- "trendPlot"
    # eventually let users alter this?
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

# ---------------------------------------------------------------------------
# html dependencies according htmltools protocols
# these are here basically so we can take advantage of shiny::createWebDependency
# ---------------------------------------------------------------------------

plotly_embed <- function() {
  htmltools::htmlDependency(name = "plotly_embed",
                            version = packageVersion("plotly"),
                            src = system.file("shiny", package = "plotly"),
                            script = "plotly_embed.js")
}

plotly_offline <- function() {
  # shiny already has jQuery (and it requires >= 1.10)
  off <- get_offline(jq = FALSE)
  list(
    htmltools::htmlDependency(name = "offline_bundle",
                              # better way to track the bundle version?
                              version = packageVersion("plotly"),
                              src = dirname(off),
                              script = basename(off)),
    htmltools::htmlDependency(name = "plotly_offline",
                              version = packageVersion("plotly"),
                              src = system.file("shiny", package = "plotly"),
                              script = "plotly_offline.js")
  )
}
