#' Hide a color bar 
#' 
#' @param p a plotly object.
#' @export
#' @examples 
#' 
#' plot_ly(economics, x = ~date, y = ~unemploy / pop, color = ~pop) %>%
#'   add_markers() %>%
#'   hide_colorbar()
hide_colorbar <- function(p) {
  p <- plotly_build(p)
  for (i in seq_along(p$x$data)) {
    trace <- p$x$data[[i]]
    if (has_attr(trace$type, "showscale")) {
      p$x$data[[i]]$showscale <- FALSE
    }
    if (has_attr(trace$type, "marker")) {
      p$x$data[[i]]$marker$showscale <- FALSE
    }
  }
  p
}

#' Convert trace types to WebGL
#' 
#' @param p a plotly or ggplot object.
#' @export
#' @examples 
#' 
#' # currently no bargl trace type
#' toWebGL(qplot(1:10))
#' toWebGL(qplot(1:10, 1:10))
#' 
toWebGL <- function(p) {
  p$x$.plotlyWebGl <- TRUE
  p
}


#' Create a complete empty plotly graph.
#' 
#' Useful when used with \link{subplot}
#' 
#' @export
plotly_empty <- function(...) {
  eaxis <- list(
    showticklabels = FALSE,
    showgrid = FALSE,
    zeroline = FALSE
  )
  layout(plot_ly(...), xaxis = eaxis, yaxis = eaxis)
}


