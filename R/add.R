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
#' @param x the x variable.
#' @param y the y variable.
#' @param text textual labels.
#' @param ymin a variable used to define the lower boundary of a polygon.
#' @param ymax a variable used to define the upper boundary of a polygon.
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
#' @rdname add_trace
#' @examples 
#' 
#' p <- plot_ly(economics, x = ~date, y = ~uempmed)
#' p
#' p %>% add_markers()
#' p %>% add_lines()
#' p %>% add_text(text = ".")
#' 
#' # attributes declared in plot_ly() carry over to downstream traces
#' plot_ly(economics, x = ~date, y = ~uempmed) %>% 
#'   add_lines(line = list(color = "red")) %>%
#'   add_markers(color = ~pop) %>%
#'   layout(showlegend = FALSE)
#' 
add_trace <- function(p, ...,
                      color, symbol, size, linetype, data = NULL) {
  # "native" plotly arguments
  attrs <- list(...)
  
  # tack on "special" arguments
  attrs$color <- if (!missing(color)) color
  attrs$symbol <- if (!missing(symbol)) symbol
  attrs$linetype <- if (!missing(linetype)) linetype
  attrs$size <- if (!missing(size)) size
  
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
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_markers <- function(p, x = NULL, y = NULL, ...) {
  if (is.null(x <- x %||% p$x$attrs[[1]][["x"]])) {
    stop("Must supply `x` attribute", call. = FALSE)
  }
  if (is.null(y <- y %||% p$x$attrs[[1]][["y"]])) {
    stop("Must supply `y` attribute", call. = FALSE)
  }
  add_trace(p, x = x, y = y, type = "scatter", mode = "markers", ...)
}

#' Add text to a plotly vis
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_text <- function(p, x = NULL, y = NULL, text = NULL, ...) {
  if (is.null(x <- x %||% p$x$attrs[[1]][["x"]])) {
    stop("Must supply `x` attribute", call. = FALSE)
  }
  if (is.null(y <- y %||% p$x$attrs[[1]][["y"]])) {
    stop("Must supply `y` attribute", call. = FALSE)
  }
  if (is.null(text <- text %||% p$x$attrs[[1]][["text"]])) {
    stop("Must supply `text` attribute", call. = FALSE)
  }
  add_trace(p, x = x, y = y, text = text, type = "scatter", mode = "text",  ...)
}

#' Add paths to a plotly vis
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_paths <- function(p, x = NULL, y = NULL, ...) {
  if (is.null(x <- x %||% p$x$attrs[[1]][["x"]])) {
    stop("Must supply `x` attribute", call. = FALSE)
  }
  if (is.null(y <- y %||% p$x$attrs[[1]][["y"]])) {
    stop("Must supply `y` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, x = x, y = y, class = "plotly_path", type = "scatter", mode = "lines", ...
  )
}

#' Add lines to a plotly vis
#' 
#' Equivalent to \code{add_paths}, but with the x-values sorted.
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' txhousing %>% 
#'   group_by(city) %>% 
#'   plot_ly(x = ~date, y = ~median) %>%
#'   add_lines(line = list(color = toRGB("black", 0.2)))
#' 
add_lines <- function(p, x = NULL, y = NULL, ...) {
  if (is.null(x <- x %||% p$x$attrs[[1]][["x"]])) {
    stop("Must supply `x` attribute", call. = FALSE)
  }
  if (is.null(y <- y %||% p$x$attrs[[1]][["y"]])) {
    stop("Must supply `y` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, x = x, y = y, class = "plotly_line", type = "scatter", mode = "lines", ...
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_segments <- function(p, x = NULL, y = NULL, xend = NULL, yend = NULL, ...) {
  if (is.null(x <- x %||% p$x$attrs[[1]][["x"]])) {
    stop("Must supply `x` attribute", call. = FALSE)
  }
  if (is.null(y <- y %||% p$x$attrs[[1]][["y"]])) {
    stop("Must supply `y` attribute", call. = FALSE)
  }
  if (is.null(xend <- xend %||% p$x$attrs[[1]][["xend"]])) {
    stop("Must supply `xend` attribute", call. = FALSE)
  }
  if (is.null(yend <- yend %||% p$x$attrs[[1]][["yend"]])) {
    stop("Must supply `yend` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, x = x, y = y, xend = xend, yend = yend,
    class = "plotly_segment", type = "scatter", mode = "lines", ...
  )
}


#' 
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' ggplot2::map_data("world", "canada") %>%
#'   group_by(group) %>%
#'   plot_ly(x = ~long, y = ~lat) %>%
#'   add_polygons(hoverinfo = "none") %>%
#'   add_markers(text = ~paste(name, "<br />", pop), hoverinfo = "text",
#'     data = maps::canada.cities) %>%
#'   layout(showlegend = FALSE)
add_polygons <- function(p, x = NULL, y = NULL, ...) {
  if (is.null(x <- x %||% p$x$attrs[[1]][["x"]])) {
    stop("Must supply `x` attribute", call. = FALSE)
  }
  if (is.null(y <- y %||% p$x$attrs[[1]][["y"]])) {
    stop("Must supply `y` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_polygon", x = x, y = y,
    type = "scatter", fill = "toself", mode = "lines",  ...
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly(economics, x = ~date) %>% 
#'   add_ribbons(ymin = ~pce - 1e3, ymax = ~pce + 1e3)

add_ribbons <- function(p, x = NULL, ymin = NULL, ymax = NULL, ...) {
  if (is.null(x <- x %||% p$x$attrs[[1]][["x"]])) {
    stop("Must supply `x` attribute", call. = FALSE)
  }
  if (is.null(ymin <- ymin %||% p$x$attrs[[1]][["ymin"]])) {
    stop("Must supply `ymin` attribute", call. = FALSE)
  }
  if (is.null(ymax <- ymax %||% p$x$attrs[[1]][["ymax"]])) {
    stop("Must supply `ymax` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = c("plotly_ribbon", "plotly_polygon"), 
    x = x, ymin = ymin, ymax = ymax,
    type = "scatter", fill = "toself", mode = "lines",  ...
  )
}

#' Area plots
#' 
#' Equivalent to \link{add_ribbon} with \code{ymin} set to 0.
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#' plot_ly(huron, x = ~year, ymax = ~level) %>% add_area()
#' 
add_area <- function(p, x = NULL, ymax = NULL, ...) {
  if (is.null(x <- x %||% p$x$attrs[[1]][["x"]])) {
    stop("Must supply `x` attribute", call. = FALSE)
  }
  if (is.null(ymax <- ymax %||% p$x$attrs[[1]][["ymax"]])) {
    stop("Must supply `ymax` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = c("plotly_area", "plotly_ribbon", "plotly_polygon"), 
    x = x, ymax = ymax,
    type = "scatter", fill = "toself", mode = "lines",  ...
  )
}


#' Bar chart
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' library(dplyr)
#' mtcars %>%
#'   count(vs) %>%
#'   plot_ly(x = ~vs, y = ~n) %>%
#'   add_bars()
#' 
add_bars <- function(p, x = NULL, y = NULL, ...) {
  x <- x %||% p$x$attrs[[1]][["x"]]
  y <- y %||% p$x$attrs[[1]][["y"]]
  if (is.null(x) && is.null(y)) {
    stop("Must supply `x` and/or `y` attributes", call. = FALSE)
  }
  # TODO: provide type checking in plotly_build for this trace type
  add_trace_classed(
    p, class = "plotly_bar", x = x, y = y, type = "bar",  ...
  )
}

#' Histogram
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly(x = ~rnorm(100)) %>% add_histogram()
#' 
add_histogram <- function(p, x = NULL, y = NULL, ...) {
  x <- x %||% p$x$attrs[[1]][["x"]]
  y <- y %||% p$x$attrs[[1]][["y"]]
  if (is.null(x) && is.null(y)) {
    stop("Must supply `x` and/or `y` attributes", call. = FALSE)
  }
  # TODO: provide type checking in plotly_build for this trace type
  add_trace_classed(
    p, class = "plotly_histogram", x = x, y = y, type = "histogram",  ...
  )
}

#' 2D Histogram
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly(x = ~LETTERS, y = ~LETTERS) %>% add_histogram2d()
#' z <- as.matrix(table(LETTERS, LETTERS))
#' plot_ly(x = ~LETTERS, y = ~LETTERS, z = ~z) %>% add_histogram2d()
#' 
add_histogram2d <- function(p, x = NULL, y = NULL, z = NULL, ...) {
  x <- x %||% p$x$attrs[[1]][["x"]]
  y <- y %||% p$x$attrs[[1]][["y"]]
  z <- z %||% p$x$attrs[[1]][["z"]]
  if (is.null(z)) {
    if (is.null(x) || is.null(y)) {
      stop("Must supply both `x` and `y` attributes if `z` is NULL", call. = FALSE)
    }
  }
  # TODO: provide type checking in plotly_build for this trace type
  add_trace_classed(
    p, class = "plotly_histogram2d", x = x, y = y, z = z,
    type = "histogram2d",  ...
  )
}

#' 2D Histogram Contour
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly(MASS::geyser, x = ~waiting, y = ~duration) %>% 
#' add_histogram2dcontour()
#' 
#' 
add_histogram2dcontour <- function(p, x = NULL, y = NULL, z = NULL, ...) {
  x <- x %||% p$x$attrs[[1]][["x"]]
  y <- y %||% p$x$attrs[[1]][["y"]]
  z <- z %||% p$x$attrs[[1]][["z"]]
  if (is.null(z)) {
    if (is.null(x) || is.null(y)) {
      stop("Must supply both `x` and `y` attributes if `z` is NULL", call. = FALSE)
    }
  }
  # TODO: provide type checking in plotly_build for this trace type
  add_trace_classed(
    p, class = "plotly_histogram2dcontour", x = x, y = y, z = z,
    type = "histogram2dcontour",  ...
  )
}


#' Heatmaps
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly(z = ~volcano) %>% add_heatmap()
#' 
add_heatmap <- function(p, z = NULL, ...) {
  if (is.null(z <- z %||% p$x$attrs[[1]][["z"]])) {
    stop("Must supply `z` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_heatmap", z = z,
    type = "heatmap",  ...
  )
}

#' Contours
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly(z = ~volcano) %>% add_contour()
#' 
add_contour <- function(p, z = NULL, ...) {
  if (is.null(z <- z %||% p$x$attrs[[1]][["z"]])) {
    stop("Must supply `z` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_contour", z = z,
    type = "contour",  ...
  )
}

#' Boxplots
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly(mtcars, x = ~factor(vs), y = ~mpg) %>% add_boxplot()
#' 
add_boxplot <- function(p, x = NULL, y = NULL, ...) {
  x <- x %||% p$x$attrs[[1]][["x"]]
  y <- y %||% p$x$attrs[[1]][["y"]]
  if (is.null(x) && is.null(y)) {
    stop("Must supply either `x` or `y` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_boxplot", x = x,  y = y, type = "box",  ...
  )
}

#' Surface
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly(z = ~volcano) %>% add_surface()
#' 
add_surface <- function(p, z = NULL, ...) {
  if (is.null(z <- z %||% p$x$attrs[[1]][["z"]])) {
    stop("Must supply `z` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_surface", z = z, type = "surface",  ...
  )
}


#' Geo
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' plot_ly() %>% add_scattergeo()
#' 
add_scattergeo <- function(p, ...) {
  add_trace_classed(
    p, class = "plotly_scattergeo", type = "scattergeo", ...
  )
}

#' Choropleths
#' 
#' @inheritParams add_trace
#' @rdname add_trace
#' @export
#' @examples 
#' 
#' density <- state.x77[, "Population"] / state.x77[, "Area"]
#' plot_ly(z = ~density) %>% 
#'   add_choropleth(locations = state.abb, locationmode = 'USA-states') %>%
#'   layout(geo = list(scope = "usa"))
#' 
add_choropleth <- function(p, z = NULL, ...) {
  if (is.null(z <- z %||% p$x$attrs[[1]][["z"]])) {
    stop("Must supply `z` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_choropleth", type = "choropleth", ...
  )
}




# attach a class to a trace which informs data processing in plotly_build
add_trace_classed <- function(p, class = "plotly_polygon", ...) {
  p <- add_trace(p, ...)
  nAttrs <- length(p$x$attrs)
  p$x$attrs[[nAttrs]] <- prefix_class(p$x$attrs[[nAttrs]], class)
  p
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


