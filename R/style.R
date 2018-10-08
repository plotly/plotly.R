
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
#' # this clobbers the previously supplied marker.line.color
#' style(p, marker.line = list(width = 2.5), marker.size = 10)
#' 
style <- function(p, ..., traces = NULL) {
  p <- plotly_build(p)
  n_traces <- length(p$x$data)
  trace_idx <- traces %||% seq_len(n_traces)
  if (any(trace_idx > n_traces)) {
    warning("You've referenced non-existent traces", call. = FALSE)
  }
  
  values <- list(...)
  paths <- strsplit(names(values), "\\.")
  
  p$x$data[trace_idx] <- lapply(p$x$data[trace_idx], function(trace) {
    for (i in seq_along(paths)) {
      trace <- trace_replace(trace, paths[[i]], values[[i]])
    }
    trace
  })
  
  p
}

#' @param trace a single plotly trace
#' @param path character vector of path elements pointing to a trace property: c("marker", "line", "size")
#' @param value a value to assign to that trace property
trace_replace <- function(trace, path, value) {
  if (length(path) == 0) return(trace)
  if (length(path) == 1) {
    trace[[path]] <- value
    return(trace)
  }
  trace[[path[1]]] <- trace[[path[1]]] %||% setNames(list(NULL), path[2])
  trace[[path[1]]] <- trace_replace(trace[[path[1]]], path[-1], value)
  trace
}
