
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
#' # style() is especially useful in conjunction with ggplotly()
#' # It allows you to leverage the underlying plotly.js library to change 
#' # the return result of ggplotly()
#' (p <- ggplotly(qplot(data = mtcars, wt, mpg, geom = c("point", "smooth"))))
#' 
#' # removes hoverinfo for the line/ribbon traces (use `plotly_json()` to verify!)
#' style(p, hoverinfo = "none", traces = c(2, 3))
#' 
#' # another example with plot_ly() instead of ggplotly()
#' marker <- list(
#'   color = "red",
#'   line = list(
#'     width = 20, 
#'     color = "black"
#'  )
#' )
#' (p <- plot_ly(x = 1:10, y = 1:10, marker = marker))
#' 
#' # note how the entire (marker) object is replaced if a list is provided
#' style(p, marker = list(line = list(color = "blue")))
#' 
#' # similar to plotly.js, you can update a particular attribute like so 
#' # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
#' style(p, marker.line.color = "blue") 
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
