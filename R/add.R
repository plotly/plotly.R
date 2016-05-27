#' Add data to a plotly visualization
#' 
#' @param p a plotly visualization
#' @param data a data frame.
#' @export
#' @examples 
#' 
#' NULL %>% plot_ly() %>% add_data(economics) %>% add_trace(x = ~date, y = ~pce)
add_data <- function(p, data = NULL) {
  if (is.null(data)) return(p)
  p <- verify_plot(p)
  id <- new_id()
  p$x$visdat[[id]] <- function() data
  p$x$cur_data <- id
  # TODO: should this also override the data used for the most recent trace?
  p
}

#' Add a trace to a plotly visualization
#' 
#' @param p a plotly or ggplot object.
#' @param ... These arguments are documented in the references section below.
#' Note that acceptable arguments depend on the trace type.
#' @param group Either a variable name or a vector to use for grouping. If used, 
#' a different trace will be created for each unique value.
#' @param color Either a variable name or a vector to use for color mapping.
#' @param colors Either a colorbrewer2.org palette name (e.g. "YlOrRd" or "Blues"), 
#' or a vector of colors to interpolate in hexadecimal "#RRGGBB" format, 
#' or a color interpolation function like \code{colorRamp}.
#' @param symbol Either a variable name or a (discrete) vector to use for symbol encoding.
#' @param symbols A character vector of symbol types. Possible values:
#' 'dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up' 
#' @param size A variable name or numeric vector to encode the size of markers.
#' @param data A data frame to associate with this trace (optional). If not 
#' provided, arguments are evaluated using the data frame in \code{\link{plot_ly}()}.
#' @seealso \code{\link{plot_ly}()}
#' @references \url{https://plot.ly/r/reference/}
#' @author Carson Sievert
#' @export
add_trace <- function(p, ...,
                      group, color, colors, symbol, symbols, size, data = NULL) {
  # "native" plotly arguments
  argz <- list(...)
  # tack on "special" arguments
  if (!missing(group)) argz$group <- substitute(group)
  if (!missing(color)) argz$color <- substitute(color)
  if (!missing(colors)) argz$colors <- substitute(colors)
  if (!missing(symbol)) argz$symbol <- substitute(symbol)
  if (!missing(symbols)) argz$symbols <- substitute(symbols)
  if (!missing(size)) argz$size <- substitute(size)
  argz$type <- verify_type(argz$type)
  
  p <- add_data(p, data)
  p$x$attrs[[p$x$cur_data]] <- argz
  p
}

#' @export
add_points <- function(p, ...) {
  add_trace(p, type = "scatter", mode = "markers", ...)
}

#' @export
add_lines <- function(p, ...) {
  add_trace(p, type = "scatter", mode = "lines", ...)
}

#' @export
add_text <- function(p, ...) {
  add_trace(p, type = "scatter", mode = "text", ...)
}

#' @export
add_polygons <- function(p, ...) {
  # TODO: Should mode='markers+lines'? If so, retrace first points?
  add_trace(p, type = "scatter", mode = "none", fill = "toself", ...)
}

