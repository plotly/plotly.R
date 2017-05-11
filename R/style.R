
#' Modify trace(s)
#'
#' Modify trace(s) of an existing plotly visualization. Useful when used in
#' conjunction with \code{\link{get_figure}()}.
#'
#' @param p A plotly visualization.
#' @param ... Visual properties.
#' @param traces numeric vector. Which traces should be modified? By default,
#' attributes place in \code{...} will be applied to every trace.
#' @seealso \code{\link{api_download_plot}()}
#' @author Carson Sievert
#' @export
#' @examples 
#' 
#' p <- qplot(data = mtcars, wt, mpg, geom = c("point", "smooth"))
#' # keep the hover info for points, but remove it for the line/ribbon
#' style(p, hoverinfo = "none", traces = c(2, 3))
#' 
style <- function(p, ..., traces = NULL) {
  p <- plotly_build(p)
  nTraces <- length(p$x$data)
  traces <- traces %||% seq_len(nTraces)
  idx <- traces > nTraces
  traces <- traces[!idx]
  if (any(idx)) warning("You've referenced non-existent traces", call. = FALSE)
  argz <- list(...)
  for (i in traces) {
    for (j in names(argz)) {
      p$x$data[[i]][[j]] <- argz[[j]]
    }
  }
  p
}
