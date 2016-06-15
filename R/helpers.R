#' Hide a color bar 
#' 
#' @param p a plotly object.
#' @export
#' 
#' plot_ly(economics, x = ~date, y = ~unemploy / pop, color = ~pop) %>%
#'   add_points() %>%
#'   hide_colorbar()
hide_colorbar <- function(p) {
  idx <- vapply(p$x$data, is.colorbar, logical(1))
  browser()
  for (i in which(idx)) {
    p$x$data[[i]]$marker$colorbar$showscale <- FALSE
  }
  p
}

hide_legend <- function(p) {
  layout(p, showlegend = FALSE)
}



#' Convert trace types to WebGL
#' 
#' @param p a plotly or ggplot object.
#' @param warn should a warning be produced if the trace type doesn't have 
#' a WebGL equivalent?
#' @export
#' @examples 
#' 
#' # currently no bargl trace type
#' toWebGL(qplot(1:10))
#' toWebGL(qplot(1:10, 1:10))
#' 
toWebGL <- function(p, warn = TRUE) {
  p <- plotly_build(p)
  types <- sapply(p$x$data, function(x) x[["type"]][1] %||% "scatter")
  idx <- paste0(types, "gl") %in% traces
  if (any(!idx) && warn) {
    warning(
      "The following traces don't have a WebGL equivalent: ",
      paste(which(!idx), collapse = ", ")
    )
  }
  for (i in which(idx)) {
    p$x$data[[i]]$type <- paste0(p$x$data[[i]]$type, "gl")
  }
  p
}

