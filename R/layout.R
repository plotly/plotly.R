#' Modify the layout of a plotly visualization
#' 
#' @param p A plotly object.
#' @param ... Arguments to the layout object. For documentation,
#' see \url{https://plot.ly/r/reference/#Layout_and_layout_style_objects}
#' @param data A data frame to associate with this layout (optional). If not 
#' provided, arguments are evaluated using the data frame in \code{\link{plot_ly}()}.
#' @author Carson Sievert
#' @export
layout <- function(p, ..., data = NULL) {
  UseMethod("layout")
}

#' @export
layout.matrix <- function(p, ..., data = NULL) {
  # workaround for the popular graphics::layout() function
  # https://github.com/ropensci/plotly/issues/464
  graphics::layout(p, ...)
}

#' @export
layout.plotly <- function(p, ..., data = NULL) {
  argz <- list(...)
  nLayouts <- length(p$x$layout)
  p$x$layout[[nLayouts + 1]] <- list(
    attrs = argz,
    rdata = data %||% p$x$data[[1]]$rdata
  )
  p
}

#' Set the default configuration for plotly
#' 
#' @param p a plotly object
#' @param ... these arguments are documented at 
#' \url{https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js}
#' @author Carson Sievert
#' @export
#' @examples \dontrun{
#' config(plot_ly(), displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud'))
#' }

config <- function(p, ...) {
  nConfig <- length(p$x$config)
  p$x$config[[nConfig + 1]] <- list(...)
  p
}
