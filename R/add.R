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
#' @examples 
#' 
#' p <- plot_ly(economics, x = ~date, y = ~uempmed)
#' # add a loess smoother
#' p2 <- add_trace(p, y = ~fitted(loess(uempmed ~ as.numeric(date))))
#' 
add_trace <- function(p, ...,
                      color, colors = NULL, symbol, symbols = NULL, 
                      size, data = NULL) {
  # "native" plotly arguments
  argz <- list(...)
  
  argz$type <- verify_type(argz$type %||% p$x$attrs[[1]]$type)
  
  if (!is.null(argz[["group"]])) {
    warning("The group argument has been deprecated. Use group_by() instead.")
  }
  
  # tack on "special" arguments
  argz$color <- verify_arg(color)
  argz$symbol <- verify_arg(symbol)
  argz$size <- verify_arg(size)
  
  argz$colors <- colors
  argz$colors <- symbols
  
  p <- add_data(p, data)
  
  # inherit arguments from the "first layer"
  new_attrs <- modifyList(p$x$attrs[[1]] %||% list(), argz)
  p$x$attrs <- c(
    p$x$attrs %||% list(),
    setNames(list(new_attrs), p$x$cur_data)
  )
  
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
#' @examples 
#' 
#' library(dplyr)
#' data(canada.cities, package = "maps")
#' 
#' ggplot2::map_data("world", "canada") %>%
#'   group_by(group) %>%
#'   plot_ly(x = ~long, y = ~lat, hoverinfo = "none") %>%
#'   add_points(text = ~paste(name, "<br />", pop), hoverinfo = "text",
#'     data = canada.cities) %>%
#'   layout(showlegend = FALSE)
add_polygons <- function(p, ...) {
  # TODO: Should mode='markers+lines'? If so, retrace first points?
  add_trace(p, type = "scatter", mode = "lines", fill = "toself", ...)
}

#' @export
add_ribbons <- function(p, ...) {
  # TODO: add ymin, ymax arguments?
  add_polygons(...)
}


#' @export
#' @examples
#' 
#' x <- rnorm(10)
#' plot_ly(x = ~x) %>%
#'   add_chull()
add_chull <- function(p, ...) {
  stop("not yet implemented")
  ch <- chull(x, y = NULL)
  # TODO: Should mode='markers+lines'? If so, retrace first points?
  add_polygons(...)
}


# ------------------------------------------------------------------------
# Non-trace addition
# ------------------------------------------------------------------------

#' @export
add_transform <- function(p, ...) {
  stop("not yet implemented")
}


#' @export
add_shape <- function(p, ...) {
  stop("not yet implemented")
}

#' @export
add_annotation <- function(p, ...) {
  stop("not yet implemented")
}


