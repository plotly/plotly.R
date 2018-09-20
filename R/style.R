
#' Modify trace(s)
#'
#' Modify trace(s) of an existing plotly visualization. Useful when used in
#' conjunction with [get_figure()].
#'
#' @param p A plotly visualization.
#' @param ... Visual properties.
#' @param traces numeric vector. Which traces should be modified? By default,
#' attributes place in `...` will be applied to every trace.
#' @seealso [api_download_plot()]
#' @author Carson Sievert
#' @export
#' @examples 
#' 
#' p <- qplot(data = mtcars, wt, mpg, geom = c("point", "smooth"))
#' # keep the hover info for points, but remove it for the line/ribbon
#' style(p, hoverinfo = "none", traces = c(2, 3))
#' 
#' 
#' # to turn the marker's red, without destroying the marker's other properties
#' # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
#' style(p, marker.color = "red", traces = 1) 
#' style(p, marker.line.color = "red", traces = 1) 
#' 
style <- function(p, ..., traces = NULL) {
  p <- plotly_build(p)
  nTraces <- length(p$x$data)
  traces <- traces %||% seq_len(nTraces)
  idx <- traces > nTraces
  traces <- traces[!idx]
  if (any(idx)) warning("You've referenced non-existent traces", call. = FALSE)
  argz <- list(...)
  
  # argument names that contain a '.' signify a "partial update"
  isPartialUpdate <- grepl("\\.", names(argz))
  # expand these special arguments to a suitable list object,
  # `list(marker.color = "red")`, to `list(marker = list(color = "red"))`
  nms <- strsplit(names(argz), "\\.")
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    if (length(nm) == 1) next
    val <- argz[[i]]
    for (j in seq(length(nm), 2)) {
      val <- setNames(list(val), nm[j])
    }
    argz[[i]] <- val
  }
  argz <- setNames(argz, unlist(lapply(nms, `[[`, 1)))
  
  # perform the replacement
  for (i in traces) {
    for (j in seq_along(argz)) {
      attr <- names(argz)[j]
      p$x$data[[i]][[attr]] <- if (isPartialUpdate[j]) modify_list(p$x$data[[i]][[attr]], argz[[attr]]) else argz[[attr]]
    }
  }
  
  p
}
