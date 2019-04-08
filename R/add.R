#' Add data to a plotly visualization
#' 
#' @param p a plotly visualization
#' @param data a data frame.
#' @export
#' @examples 
#' 
#' plot_ly() %>% add_data(economics) %>% add_trace(x = ~date, y = ~pce)
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
#' @inheritParams plot_ly
#' @param p a plotly object
#' @param inherit inherit attributes from [plot_ly()]?
#' @param z a numeric matrix
#' @param x the x variable.
#' @param y the y variable.
#' @param text textual labels.
#' @param ymin a variable used to define the lower boundary of a polygon.
#' @param ymax a variable used to define the upper boundary of a polygon.
#' @param xend "final" x position (in this context, x represents "start")
#' @param yend "final" y position (in this context, y represents "start")
#' @seealso [plot_ly()]
#' @references \url{https://plotly-r.com/overview.html}
#' 
#' \url{https://plot.ly/r}
#' 
#' \url{https://plot.ly/r/reference/} 
#' @author Carson Sievert
#' @export
#' @rdname add_trace
#' @examples 
#' 
#' # the `plot_ly()` function initiates an object, and if no trace type
#' # is specified, it sets a sensible default
#' p <- plot_ly(economics, x = ~date, y = ~uempmed)
#' p
#' 
#' # some `add_*()` functions are a specific case of a trace type
#' # for example, `add_markers()` is a scatter trace with mode of markers
#' add_markers(p)
#' 
#' # scatter trace with mode of text
#' add_text(p, text = "%")
#' 
#' # scatter trace with mode of lines 
#' add_paths(p)
#' 
#' # like `add_paths()`, but ensures points are connected according to `x`
#' add_lines(p)
#' 
#' # if you prefer to work with plotly.js more directly, can always
#' # use `add_trace()` and specify the type yourself
#' add_trace(p, type = "scatter", mode = "markers+lines")
#' 
#' # mappings provided to `plot_ly()` are "global", but can be overwritten
#' plot_ly(economics, x = ~date, y = ~uempmed, color = I("red"), showlegend = FALSE) %>% 
#'   add_lines() %>%
#'   add_markers(color = ~pop)
#' 
#' # a number of `add_*()` functions are special cases of the scatter trace
#' plot_ly(economics, x = ~date) %>% 
#'   add_ribbons(ymin = ~pce - 1e3, ymax = ~pce + 1e3)
#'
#' # use `group_by()` (or `group2NA()`) to apply visual mapping
#' # once per group (e.g. one line per group)
#' txhousing %>% 
#'   group_by(city) %>% 
#'   plot_ly(x = ~date, y = ~median) %>%
#'   add_lines(color = I("black"))
#' 
#' \dontrun{
#' # use `add_sf()` or `add_polygons()` to create geo-spatial maps
#' # http://blog.cpsievert.me/2018/03/30/visualizing-geo-spatial-data-with-sf-and-plotly/
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'   plot_ly() %>% add_sf(data = nc)
#' }
#' 
#' # univariate summary statistics
#' plot_ly(mtcars, x = ~factor(vs), y = ~mpg) %>% 
#'   add_boxplot()
#' plot_ly(mtcars, x = ~factor(vs), y = ~mpg) %>% 
#'   add_trace(type = "violin")
#'   
#' # `add_histogram()` does binning for you...
#' mtcars %>%
#'   plot_ly(x = ~factor(vs)) %>%
#'   add_histogram()
#'   
#' # ...but you can 'pre-compute' bar heights in R
#' mtcars %>%
#'   dplyr::count(vs) %>%
#'   plot_ly(x = ~vs, y = ~n) %>%
#'   add_bars()
#'
#' # the 2d analogy of add_histogram() is add_histogram2d()/add_histogram2dcontour()
#' library(MASS)
#' (p <- plot_ly(geyser, x = ~waiting, y = ~duration))
#' add_histogram2d(p)
#' add_histogram2dcontour(p)
#' 
#' # the 2d analogy of add_bars() is add_heatmap()/add_contour()
#' # (i.e., bin counts must be pre-specified)
#' den <- kde2d(geyser$waiting, geyser$duration)
#' p <- plot_ly(x = den$x, y = den$y, z = den$z)
#' add_heatmap(p)
#' add_contour(p)
#' 
#' # `add_table()` makes it easy to map a data frame to the table trace type
#' plot_ly(economics) %>% 
#'   add_table()
#' 
#' # pie charts!
#' ds <- data.frame(labels = c("A", "B", "C"), values = c(10, 40, 60))
#' plot_ly(ds, labels = ~labels, values = ~values) %>%
#'   add_pie() %>%
#'   layout(title = "Basic Pie Chart using Plotly")
#'   
#' data(wind)
#' plot_ly(wind, r = ~r, t = ~t) %>% 
#'   add_area(color = ~nms) %>%
#'   layout(radialaxis = list(ticksuffix = "%"), orientation = 270)
#' 
#' # ------------------------------------------------------------
#' # 3D chart types
#' # ------------------------------------------------------------
#' plot_ly(z = ~volcano) %>% 
#'   add_surface()
#' plot_ly(x = c(0, 0, 1), y = c(0, 1, 0), z = c(0, 0, 0)) %>% 
#'   add_mesh()
#' }
#' 
add_trace <- function(p, ...,
                      data = NULL, inherit = TRUE) {
  
  # "native" plotly arguments
  attrs <- list(...)
  attrs$inherit <- inherit
  
  if (!is.null(attrs[["group"]])) {
    warning("The group argument has been deprecated. Use group_by() or split instead.")
  }
  
  p <- add_data(p, data)
  
  # inherit attributes from the "first layer" (except the plotly_eval class)
  if (inherit) {
    attrs <- modify_list(unclass(p$x$attrs[[1]]), attrs)
  }
  
  p$x$attrs <- c(
    p$x$attrs %||% list(), 
    setNames(list(attrs), p$x$cur_data)
  )
  
  p
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_markers <- function(p, x = NULL, y = NULL, z = NULL, ..., 
                        data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(x) || is.null(y)) {
    stop("Must supply `x` and `y` attributes", call. = FALSE)
  }
  type <- if (!is.null(z)) "scatter3d" else "scatter"
  add_trace(
    p, x = x, y = y, z = z, type = type, mode = "markers", ...,
    data = data, inherit = inherit
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_text <- function(p, x = NULL, y = NULL, z = NULL, text = NULL, ...,
                     data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
    text <- text %||% p$x$attrs[[1]][["text"]]
  }
  if (is.null(x) || is.null(y) || is.null(text)) {
    stop("Must supply `x`, `y` and `text` attributes", call. = FALSE)
  }
  type <- if (!is.null(z)) "scatter3d" else "scatter"
  add_trace(p, x = x, y = y, z = z, text = text, type = type, mode = "text", 
            ..., data = data, inherit = inherit)
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_paths <- function(p, x = NULL, y = NULL, z = NULL, ...,
                      data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(x) || is.null(y)) {
    stop("Must supply `x` and `y` attributes", call. = FALSE)
  }
  type <- if (!is.null(z)) "scatter3d" else "scatter"
  add_trace_classed(
    p, x = x, y = y, z = z, class = "plotly_path", type = type, mode = "lines", 
    ..., data = data, inherit = inherit
  )
}

#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_lines <- function(p, x = NULL, y = NULL, z = NULL, ...,
                      data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(x) || is.null(y)) {
    stop("Must supply `x` and `y` attributes", call. = FALSE)
  }
  type <- if (!is.null(z)) "scatter3d" else "scatter"
  add_trace_classed(
    p, x = x, y = y, class = "plotly_line", type = type, mode = "lines", 
    ..., data = data, inherit = inherit
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_segments <- function(p, x = NULL, y = NULL, xend = NULL, yend = NULL, ...,
                         data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    xend <- xend %||% p$x$attrs[[1]][["xend"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    yend <- yend %||% p$x$attrs[[1]][["yend"]]
  }
  if (is.null(x) || is.null(y) || is.null(xend) || is.null(yend)) {
    stop("Must supply `x`/`y`/`xend`/`yend` attributes", call. = FALSE)
  }
  add_trace_classed(
    p, x = x, y = y, xend = xend, yend = yend,
    class = "plotly_segment", type = "scatter", mode = "lines", 
    ..., data = data, inherit = inherit
  )
}



#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_polygons <- function(p, x = NULL, y = NULL, ...,
                         data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
  }
  if (is.null(x) || is.null(y)) {
    stop("Must supply `x`/`y` attributes", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_polygon", x = x, y = y,
    type = "scatter", fill = "toself", mode = "lines",  
    ..., data = data, inherit = inherit
  )
}



#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_sf <- function(p, ..., x = ~x, y = ~y, data = NULL, inherit = TRUE) {
  try_library("sf", "add_sf")
  dat <- plotly_data(add_data(p, data))
  if (!is_sf(dat)) {
    stop(
      "The `data` for an `add_sf()` layer must be an sf object, ", 
      "not an object of class: ", class(dat)[1],
      call. = FALSE
    )
  }
  if (is_mapbox(p) || is_geo(p)) dat <- st_cast_crs(dat)
  bbox <- sf::st_bbox(dat)
  set <- attr(dat, "set")
  
  d <- to_basic.GeomSf(dat)
  
  # to_basic() returns either a single data frame or a list of data frames
  # (each data frame should be a a collection of the same feature type)
  d <- if (is.data.frame(d)) list(d)
  
  # inherit attributes from the "first layer" (except the plotly_eval class)
  attrz <- if (inherit) modify_list(unclass(p$x$attrs[[1]]), list(...)) else list(...)
  
  for (i in seq_along(d)) {
    # sensible mode/style defaults based on the feature type (e.g. polygon, path, point)
    attrs <- modify_list(sf_default_attrs(d[[i]]), attrz)
    # scatter3d doesn't currently support fill
    if ("z" %in% names(attrs)) attrs$fill <- NULL
    args <- list(
      p = p, 
      class = "plotly_sf", 
      x = x,
      y = y,
      `_bbox` = bbox,
      set = set,
      data = if ("group" %in% names(d[[i]])) group_by_(d[[i]], "group", add = TRUE) else d[[i]], 
      inherit = inherit
    )
    args <- modify_list(args, attrs)
    p <- do.call(add_trace_classed, args)
  }
  
  p
}


#' @inheritParams add_trace
#' @param rownames whether or not to display the rownames of `data`.
#' @rdname add_trace
#' @export
add_table <- function(p, ..., rownames = TRUE, data = NULL, inherit = TRUE) {
  attrs <- list(...)
  dat <- plotly_data(add_data(p, data))
  if (is.data.frame(dat)) {
    vals <- lapply(names(dat), function(x) list(x))
    if (isTRUE(rownames)) vals <- c(list(""), vals)
    header_defaults <- list(
      values = vals,
      align = "right"
    )
    vals <- setNames(as.list(dat), NULL)
    if (isTRUE(rownames)) vals <- c(list(row.names(dat)), vals)
    cell_defaults <- list(
      values = vals,
      align = "right"
    )
    attrs$header <- modify_list(header_defaults, attrs$header)
    attrs$cells <- modify_list(cell_defaults, attrs$cells)
  }
  
  do.call(add_trace_classed, c(list(p = p, class = "plotly_table", type = "table"), attrs))
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_ribbons <- function(p, x = NULL, ymin = NULL, ymax = NULL, ...,
                        data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    ymin <- ymin %||% p$x$attrs[[1]][["ymin"]]
    ymax <- ymax %||% p$x$attrs[[1]][["ymax"]]
  }
  if (is.null(x) || is.null(ymin) || is.null(ymax)) {
    stop("Must supply `x`/`ymin`/`ymax` attributes", call. = FALSE)
  }
  add_trace_classed(
    p, class = c("plotly_ribbon", "plotly_polygon"), 
    x = x, ymin = ymin, ymax = ymax, type = "scatter", mode = "lines",
    hoveron = "points", fill = "toself",  ..., data = data, inherit = inherit
  )
}

#' @inheritParams add_trace
#' @rdname add_trace
#' @param r For polar chart only. Sets the radial coordinates.
#' @param t For polar chart only. Sets the radial coordinates.
#' @export
add_area <- function(p, r = NULL, t = NULL, ...,
                     data = NULL, inherit = TRUE) {
  if (inherit) {
    r <- t %||% p$x$attrs[[1]][["r"]]
    t <- t %||% p$x$attrs[[1]][["t"]]
  }
  if (is.null(r) || is.null(t)) {
    stop("Must supply `r`/`t` attributes", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_area", r = r, t = t, type = "area",
    ..., data = data, inherit = inherit
  )
}

#' @inheritParams add_trace
#' @rdname add_trace
#' @param values the value to associated with each slice of the pie.
#' @param labels the labels (categories) corresponding to `values`.
#' @export
add_pie <- function(p, values = NULL, labels = NULL, ...,
                     data = NULL, inherit = TRUE) {
  if (inherit) {
    values <- values %||% p$x$attrs[[1]][["values"]]
    labels <- labels %||% p$x$attrs[[1]][["labels"]]
  }
  if (is.null(values)) {
    stop("Must supply `values`", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_pie", values = values, labels = labels, type = "pie",
    ..., data = data, inherit = inherit
  )
}

#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_bars <- function(p, x = NULL, y = NULL, ...,
                     data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
  }
  if (is.null(x) || is.null(y)) {
    stop("Must supply `x`/`y` attributes", call. = FALSE)
  }
  # TODO: provide type checking in plotly_build for this trace type
  add_trace_classed(
    p, class = "plotly_bar", x = x, y = y, type = "bar", 
    ..., data = data, inherit = inherit
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_histogram <- function(p, x = NULL, y = NULL, ...,
                          data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
  }
  if (is.null(x) && is.null(y)) {
    stop("Must supply `x` and/or `y` attributes", call. = FALSE)
  }
  # TODO: provide type checking in plotly_build for this trace type
  add_trace_classed(
    p, class = "plotly_histogram", x = x, y = y, type = "histogram", 
    ..., data = data, inherit = inherit
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_histogram2d <- function(p, x = NULL, y = NULL, z = NULL, ...,
                            data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(z)) {
    if (is.null(x) || is.null(y)) {
      stop("Must supply both `x` and `y` attributes if `z` is NULL", call. = FALSE)
    }
  }
  # TODO: provide type checking in plotly_build for this trace type
  add_trace_classed(
    p, class = "plotly_histogram2d", x = x, y = y, z = z,
    type = "histogram2d",  ..., data = data, inherit = inherit
  )
}

#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_histogram2dcontour <- function(p, x = NULL, y = NULL, z = NULL, ...,
                                   data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(z)) {
    if (is.null(x) || is.null(y)) {
      stop("Must supply both `x` and `y` attributes if `z` is NULL", call. = FALSE)
    }
  }
  # TODO: provide type checking in plotly_build for this trace type
  add_trace_classed(
    p, class = "plotly_histogram2dcontour", x = x, y = y, z = z,
    type = "histogram2dcontour",  ..., data = data, inherit = inherit
  )
}



#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_heatmap <- function(p, x = NULL, y = NULL, z = NULL, ..., 
                        data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(z)) {
    stop("Must supply `z` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_heatmap", z = z, x = x, y = y,
    type = "heatmap",  ..., data = data, inherit = inherit
  )
}

#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_contour <- function(p, z = NULL, ..., data = NULL, inherit = TRUE) {
  if (inherit) {
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(z)) {
    stop("Must supply `z` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_contour", z = z, type = "contour",  ..., 
    data = data, inherit = inherit
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_boxplot <- function(p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
  }
  if (is.null(x) && is.null(y)) {
    stop("Must supply either `x` or `y` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_boxplot", x = x,  y = y, type = "box", 
    ..., data = data, inherit = inherit
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_surface <- function(p, z = NULL, ..., data = NULL, inherit = TRUE) {
  if (inherit) {
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(z)) {
    stop("Must supply `z` attribute", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_surface", z = z, type = "surface", 
    ..., data = data, inherit = inherit
  )
}

#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_mesh <- function(p, x = NULL, y = NULL, z = NULL, ..., 
                        data = NULL, inherit = TRUE) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  if (is.null(x) || is.null(y) || is.null(z)) {
    stop("Must supply `x`/`y`/`z` attributes", call. = FALSE)
  }
  add_trace_classed(
    p, class = "plotly_mesh", x = x, y = y, z = z, type = "mesh3d", 
    ..., data = data, inherit = inherit
  )
}


#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_scattergeo <- function(p, ...) {
  .Deprecated("geo")
  p
}

#' @inheritParams add_trace
#' @rdname add_trace
#' @export
add_choropleth <- function(p, z = NULL, ..., 
                           data = NULL, inherit = TRUE) {
  .Deprecated("geo")
  p
}

# attach a class to a trace which informs data processing in plotly_build
add_trace_classed <- function(p, class = "plotly_polygon", ...) {
  p <- add_trace(p, ...)
  nAttrs <- length(p$x$attrs)
  p$x$attrs[[nAttrs]] <- prefix_class(p$x$attrs[[nAttrs]], class)
  p
}

# retrieve the non-plotly.js attributes for a given trace
special_attrs <- function(trace) {
  attrs <- switch(
    class(trace)[[1]],
    plotly_segment = c("xend", "yend"),
    plotly_ribbon = c("ymin", "ymax")
  )
  # for data training, we temporarily rename lat/lon as x/y
  if (isTRUE(trace[["type"]] %in% c("scattermapbox", "scattergeo"))) {
    attrs <- c(attrs, c("x", "y"))
  }
  attrs
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





#' Apply function to plot, without modifying data
#' 
#' Useful when you need two or more layers that apply a summary statistic
#' to the original data.
#' 
#' @param p a plotly object.
#' @param fun a function. Should take a plotly object as input and return a 
#' modified plotly object.
#' @param ... arguments passed to `fun`.
#' @export
add_fun <- function(p, fun, ...) {
  oldDat <- p$x$cur_data
  p <- fun(p, ...)
  p$x$cur_data <- oldDat
  p$x$attrs[length(p$x$attrs)] <- setNames(
    list(p$x$attrs[[length(p$x$attrs)]]), oldDat
  )
  p
}


#' Add an annotation(s) to a plot
#' 
#' @param p a plotly object
#' @param text annotation text (required).
#' @param ... these arguments are documented at 
#' \url{https://github.com/plotly/plotly.js/blob/master/src/components/annotations/attributes.js}
#' @param data a data frame.
#' @param inherit inherit attributes from [plot_ly()]?
#' @author Carson Sievert
#' @export
add_annotations <- function(p, text = NULL, ..., data = NULL, inherit = TRUE) {
  p <- add_data(p, data)
  attrs <- list(text = text, ...)
  # x/y/text inherit from plot_ly()
  for (i in c("x", "y", "text")) {
    attrs[[i]] <- attrs[[i]] %||% p$x$attrs[[1]][[i]]
  }
  if (is.null(attrs[["text"]])) {
    stop("Must supply text to annotation", call. = FALSE)
  }
  attrs <- list(annotations = attrs)
  # similar to layout()
  p$x$layoutAttrs <- c(
    p$x$layoutAttrs %||% list(), 
    setNames(list(attrs), p$x$cur_data)
  )
  p
}
