#' Modify the colorbar
#' 
#' @param p a plotly object
#' @param ... arguments are documented here 
#' \url{https://plot.ly/r/reference/#scatter-marker-colorbar}.
#' @author Carson Sievert
#' @export
#' @examples 
#' 
#' plot_ly(mpg, x = ~cty, y = ~hwy, color = ~cyl) %>%
#'   colorbar(len = 0.5)
#' 
colorbar <- function(p, ...) {
  p <- plotly_build(p)
  isBar <- vapply(p$x$data, is.colorbar, logical(1))
  if (sum(isBar) != 1) {
    stop("This function only works with one colorbar")
  }
  tr <- p$x$data[[which(isBar)]]
  if (inherits(tr, "zcolor")) {
    p$x$data[[which(isBar)]][["colorbar"]] <- modify_list(
      tr[["colorbar"]], list(...)
    )
  } else {
    p$x$data[[which(isBar)]]$marker$colorbar <- modify_list(
      tr$marker$colorbar, list(...)
    )
  }
  p
}


#' Hide guides (legends and colorbars)
#'
#' @param p a plotly object.
#' @export
#' @seealso \code{\link{hide_legend}()}, \code{\link{hide_colorbar}()}
#'

hide_guides <- function(p) {
  hide_legend(hide_colorbar(p))
}


#' Hide color bar(s)
#' 
#' @param p a plotly object.
#' @export
#' @seealso \code{\link{hide_legend}()}
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

#' Hide legend
#' 
#' @param p a plotly object.
#' @export
#' @seealso \code{\link{hide_legend}()}
#' @examples 
#' 
#' plot_ly(economics, x = ~date, y = ~unemploy / pop, color = ~pop) %>%
#'   add_markers() %>%
#'   hide_colorbar()

hide_legend <- function(p) {
  p <- plotly_build(p)
  # annotations have to be an array of objects, so this should be a list of lists
  ann <- p$x$layout$annotations
  for (i in seq_along(ann)) {
    if (isTRUE(ann[[i]]$legendTitle)) {
      p$x$layout$annotations[[i]] <- NULL
    }
  }
  if (length(p$x$layout$annotations) == 0) {
    p$x$layout$annotations <- NULL
  }
  p$x$layout$showlegend <- FALSE
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
  if (ggplot2::is.ggplot(p)) {
    p <- plotly_build(p)
  }
  p$x$.plotlyWebGl <- TRUE
  p
}


#' Create a complete empty plotly graph.
#' 
#' Useful when used with \code{\link{subplot}()}
#' 
#' @param ... arguments passed onto \code{\link{plot_ly}()}
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


