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
  if (!is.plotly(p)) {
    stop("Don't know how to add traces to an object of class: ", 
         class(p), call. = FALSE)
  }
  id <- new_id()
  p$x$visdat[[id]] <- function() data
  p$x$cur_data <- id
  # TODO: should this also override the data used for the most recent trace?
  p
}

#' Add trace(s) to a plotly visualization
#' 
#' @param p a plotly or ggplot object.
#' @param ... These arguments are documented in the references section below.
#' Note that acceptable arguments depend on the trace type.
#' @param color Either a variable name or a vector to use for color mapping.
#' @param symbol Either a variable name or a (discrete) vector to use for symbol encoding.
#' @param size A variable name or numeric vector to encode the size of markers.
#' @param linetype Either a variable name or a (discrete) vector to use for linetype encoding.
#' @param data A data frame to associate with this trace (optional). If not 
#' provided, arguments are evaluated using the data frame in \code{\link{plot_ly}()}.
#' @seealso \code{\link{plot_ly}()}
#' @references \url{https://plot.ly/r/reference/}
#' @author Carson Sievert
#' @export
#' @examples 
#' 
#' p <- plot_ly(economics, x = ~date, y = ~uempmed)
#' p
#' p %>% add_points()
#' p %>% add_lines()
#' p %>% add_text(text = ".")
#' 
#' # attributes declared in plot_ly() carry over to downstream traces
#' plot_ly(economics, x = ~date, y = ~uempmed) %>% 
#'   add_points(color = ~pop) %>%
#'   add_lines(line = list(color = "red"))
#'   
#' 
add_trace <- function(p, ...,
                      color, symbol, size, linetype, data = NULL) {
  # "native" plotly arguments
  attrs <- list(...)
  
  # tack on "special" arguments
  attrs$color <- verify_arg(color)
  attrs$symbol <- verify_arg(symbol)
  attrs$size <- verify_arg(size)
  
  attrs$colors <- colors
  attrs$symbols <- symbols
  
  if (!is.null(attrs[["group"]])) {
    warning("The group argument has been deprecated. Use group_by() instead.")
  }
  
  p <- add_data(p, data)
  
  # inherit attributes from the "first layer"
  new_attrs <- modify_list(p$x$attrs[[1]], attrs)
  
  p$x$attrs <- c(
    p$x$attrs %||% list(), 
    setNames(list(new_attrs), p$x$cur_data)
  )
  
  p
}

#' Add points to a plotly vis
#' 
#' @export
add_points <- function(p, ...) {
  add_trace(p, type = "scatter", mode = "markers", ...)
}

#' Add lines to a plotly vis
#' 
#' @export
add_lines <- function(p, ...) {
  add_trace(p, type = "scatter", mode = "lines", ...)
}

#' Add text to a plotly vis
#' 
#' @export
add_text <- function(p, ...) {
  # TODO: throw error if no text attribute is found
  add_trace(p, type = "scatter", mode = "text", ...)
}

#' Add polygons to a plotly vis
#' 
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

#' Add ribbons to a plotly vis
#' 
#' Ribbons are a special case of polygons.
#' 
#' @export
add_ribbons <- function(p, ...) {
  # TODO: add ymin, ymax arguments?
  add_polygons(...)
}


# #' 
# #' 
# #' @export
# #' @examples
# #' 
# #' x <- rnorm(10)
# #' plot_ly(x = ~x) %>%
# #'   add_chull()
# add_chull <- function(p, ...) {
#   stop("not yet implemented")
#   ch <- chull(x, y = NULL)
#   # TODO: Should mode='markers+lines'? If so, retrace first points?
#   add_polygons(...)
# }


## ------------------------------------------------------------------------
## Non-trace addition
## ------------------------------------------------------------------------
#
##' @export
#add_transform <- function(p, ...) {
#  stop("not yet implemented")
#}
#
#
##' @export
#add_shape <- function(p, ...) {
#  stop("not yet implemented")
#}
#
##' @export
#add_annotation <- function(p, ...) {
#  stop("not yet implemented")
#}


