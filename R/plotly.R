#' Initiate a plotly visualization
#'
#' Transform data into a plotly visualization.
#' 
#' There are a number of "visual properties" that aren't included in the officical 
#' Reference section (see below). 
#' 
#' @param data A data frame (optional).
#' @param ... These arguments are documented at \url{https://plot.ly/r/reference/}
#' Note that acceptable arguments depend on the value of \code{type}.
#' @param type A character string describing the type of trace.
#' @param group Either a variable name or a vector to use for grouping. If used, 
#' a different trace will be created for each unique value.
#' @param color Either a variable name or a vector to use for color mapping.
#' @param colors Either a colorbrewer2.org palette name (e.g. "YlOrRd" or "Blues"), 
#' or a vector of colors to interpolate in hexadecimal "#RRGGBB" format, 
#' or a color interpolation function like \code{colorRamp()}.
#' @param symbol Either a variable name or a (discrete) vector to use for symbol encoding.
#' @param symbols A character vector of symbol types. Possible values:
#' 'dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up' 
#' @param size A variable name or numeric vector to encode the size of markers.
#' @param width	Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @param inherit logical. Should future traces inherit properties from this initial trace?
#' @param evaluate logical. Evaluate arguments when this function is called?
#' @param source Only relevant for \link{event_data}.
#' @seealso \code{\link{layout}()}, \code{\link{add_trace}()}, \code{\link{style}()}
#' @author Carson Sievert
#' @export
#' @examples
#' \dontrun{
#' data(economics, package = "ggplot2")
#' # basic time-series plot
#' p <- plot_ly(economics, x = date, y = uempmed, type = "scatter", 
#'   showlegend = FALSE)
#' # add a loess smoother
#' p2 <- add_trace(p, y = fitted(loess(uempmed ~ as.numeric(date))))
#' # add a title
#' p3 <- layout(p2, title = "Median duration of unemployment (in weeks)")
#' # change the font
#' layout(p3, font = list(family = "Courier New, monospace"))
#' 
#' # using the color argument
#' plot_ly(economics, x = date, y = unemploy / pop, color = pop, mode = "markers")
#' plot_ly(economics, x = date, y = unemploy / pop, color = pop, 
#'   colors = terrain.colors(5), mode = "markers")
#'   
#' # function to extract the decade of a given date
#' decade <- function(x) {
#'   factor(floor(as.numeric(format(x, "%Y")) / 10) * 10)
#' }
#' plot_ly(economics, x = unemploy / pop, color = decade(date), type = "box")
#' 
#' # plotly loves pipelines
#' economics %>%
#'  transform(rate = unemploy / pop) %>%
#'  plot_ly(x = date, y = rate) %>%
#'  loess(rate ~ as.numeric(date), data = .) %>%
#'  broom::augment() %>%
#'  add_trace(y = .fitted)
#' 
#' # sometimes, a data frame isn't fit for the use case...
#' # for 3D surface plots, a numeric matrix is more natural
#' plot_ly(z = volcano, type = "surface")
#' }
#' 
plot_ly <- function(data = data.frame(), ..., type = "scatter",
                    group, color, colors, symbol, symbols, size,
                    width = NULL, height = NULL, inherit = FALSE, 
                    evaluate = FALSE, source = "A") {
  # "native" plotly arguments
  argz <- substitute(list(...))
  # old arguments to this function that are no longer supported
  if (!is.null(argz$filename)) 
    warning("Ignoring filename. Use plotly_POST() if you want to post figures to plotly.")
  if (!is.null(argz$fileopt)) 
    warning("Ignoring fileopt. Use plotly_POST() if you want to post figures to plotly.")
  if (!is.null(argz$world_readable)) 
    warning("Ignoring world_readable. Use plotly_POST() if you want to post figures to plotly.")
  # tack on "special" arguments
  if (!missing(group)) argz$group <- substitute(group)
  if (!missing(color)) argz$color <- substitute(color)
  if (!missing(colors)) argz$colors <- substitute(colors)
  if (!missing(symbol)) argz$symbol <- substitute(symbol)
  if (!missing(symbols)) argz$symbols <- substitute(symbols)
  if (!missing(size)) argz$size <- substitute(size)
  # trace information
  tr <- list(
    type = type,
    args = argz,
    env = list2env(data),    # environment in which to evaluate arguments
    enclos = parent.frame(), # if objects aren't found in env, look here
    inherit = inherit
  )
  # plotly objects should always have a _list_ of trace(s)
  p <- list(
    data = list(tr),
    layout = NULL,
    url = NULL,
    width = width,
    height = height,
    source = source
  )
  
  if (evaluate) p <- plotly_build(p)
  hash_plot(data, p)
}

#' Add a trace to a plotly visualization
#' 
#' @param p A plotly object.
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
#' @param evaluate logical. Evaluate arguments when this function is called?
#' @seealso \code{\link{plot_ly}()}
#' @references \url{https://plot.ly/r/reference/}
#' @author Carson Sievert
#' @export
add_trace <- function(p = last_plot(), ...,
                      group, color, colors, symbol, symbols, size,
                      data = NULL, evaluate = FALSE) {
  # "native" plotly arguments
  argz <- substitute(list(...))
  # tack on "special" arguments
  if (!missing(group)) argz$group <- substitute(group)
  if (!missing(color)) argz$color <- substitute(color)
  if (!missing(colors)) argz$colors <- substitute(colors)
  if (!missing(symbol)) argz$symbol <- substitute(symbol)
  if (!missing(symbols)) argz$symbols <- substitute(symbols)
  if (!missing(size)) argz$size <- substitute(size)
  data <- data %||% if (is.data.frame(p)) p else list()
  tr <- list(
    args = argz,
    env = list2env(data),
    enclos = parent.frame()
  )
  p <- last_plot(p)
  p$data <- c(p$data, list(tr))
  if (evaluate) p <- plotly_build(p)
  hash_plot(data, p)
}

#' Add and/or modify layout of a plotly
#' 
#' @param p A plotly object.
#' @param ... Arguments to the layout object. For documentation,
#' see \url{https://plot.ly/r/reference/#Layout_and_layout_style_objects}
#' @param data A data frame to associate with this layout (optional). If not 
#' provided, arguments are evaluated using the data frame in \code{\link{plot_ly}()}.
#' @param evaluate logical. Evaluate arguments when this function is called?
#' @author Carson Sievert
#' @export
layout <- function(p = last_plot(), ..., 
                   data = NULL, evaluate = FALSE) {
  data <- data %||% if (is.data.frame(p)) p else list()
  layout <- list(
    args = substitute(list(...)),
    env = list2env(data),
    enclos = parent.frame()
  )
  p <- last_plot(p)
  p$layout <- c(p$layout, list(layout = layout))
  if (evaluate) p <- plotly_build(p)
  hash_plot(data, p)
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

config <- function(p = last_plot(), ...) {
  conf <- list(...)
  p <- last_plot(p)
  p$config <- c(p$config, conf)
  hash_plot(if (is.data.frame(p)) p else list(), p)
}

#' Modify trace(s)
#'
#' Modify trace(s) of an existing plotly visualization. Useful when used in
#' conjunction with \code{\link{get_figure}()}.
#'
#' @param p A plotly visualization.
#' @param ... Visual properties.
#' @param traces numeric vector. Which traces should be modified?
#' @param evaluate logical. Evaluate arguments when this function is called?
#' @seealso \code{\link{get_figure}()}
#' @author Carson Sievert
#' @export
style <- function(p = last_plot(), ..., traces = 1, evaluate = FALSE) {
  idx <- traces >= length(p$data)
  if (any(idx)) warning("You've referenced non-existent traces", call. = FALSE)
  style <- list(
    args = substitute(list(...)),
    # not optimal....
    env = p$data[[max(traces)]]$env,
    enclos = parent.frame(),
    traces = traces
  )
  p$style <- c(p$style, list(style))
  if (evaluate) p <- plotly_build(p)
  hash_plot(data, p)
}

#' Build a plotly object before viewing it
#' 
#' For convenience and efficiency purposes, plotly objects are subject to lazy 
#' evaluation. That is, the actual content behind a plotly object is not 
#' created until it is absolutely necessary. In some instances, you may want 
#' to perform this evaluation yourself, and work directly with the resulting 
#' list.
#' 
#' @param l a ggplot object, or a plotly object, or a list.
#' @export
plotly_build <- function(l = last_plot()) {
  #if (inherits(l, "ggmatrix"))
  # ggplot objects don't need any special type of handling
  if (ggplot2::is.ggplot(l)) return(gg2list(l))
  l <- get_plot(l)
  # assume unnamed list elements are data/traces
  nms <- names(l)
  idx <- nms %in% ""
  l <- if (is.null(nms)) {
    list(data = l) 
  } else if (any(idx)) {
    c(data = c(l$data, l[idx]), l[!idx])
  } else l
  # carry over properties, if necessary (but don't carry over evaluation envir)
  if (length(l$data) > 1 && isTRUE(l$data[[1]]$inherit)) {
    d <- l$data[[1]]
    d <- d[!names(d) %in% c("env", "enclos")]
    for (i in seq.int(2, length(l$data))) {
      l$data[[i]] <- modifyList(l$data[[i]], d)
    }
  }
  # 'x' is the same as 'l', but with arguments evaluated
  # this is ugly, but I think it is necessary, since we don't know how many 
  # traces we have until we evaluate args and call traceify() (or similar)
  x <- list()
  for (i in seq_along(l$data)) {
    d <- l$data[[i]]
    if (should_eval(d)) {
      dat <- do_eval(d)
      # start processing specially named arguments
      s <- dat[["size"]]
      if (!is.null(s)) {
        if (!is.numeric(s)) warning("size should be numeric", call. = FALSE)
        # if autosizing is used, guess that the plot is 300 by 600
        auto <- dat[["layout"]][["autosize"]] %||% TRUE
        hw <- if (auto) c(300, 600)
        else c(dat[["layout"]][["height"]], dat[["layout"]][["width"]])
        # ensure that markers cover 30% of the plot area
        m <- list(
          size = 0.3 * prod(hw) * (s/sum(s)),
          sizemode = "area"
        )
        # the marker object is the only type of object which respects size
        dat[["marker"]] <- modifyList(dat[["marker"]] %||% list(), m)
        # either add some appropriate hover text
        txt <- paste0(as.list(d$args)[["size"]], " (size): ", s)
        dat[["text"]] <- if (is.null(dat[["text"]])) txt else paste0(dat[["text"]], "<br>", txt)
      }
      has_color <- !is.null(dat[["color"]]) || 
        isTRUE(!is.null(dat[["z"]]) && !dat[["type"]] %in% "scatter3d")
      has_symbol <- !is.null(dat[["symbol"]])
      has_group <- !is.null(dat[["group"]])
      if (has_color) {
        title <- as.list(d$args)[["color"]] %||% as.list(d$args)[["z"]] %||% ""
        x$data <- c(x$data, colorize(dat, title))
      }
      # TODO: add a legend title (is this only possible via annotations?!?)
      if (has_symbol) x$data <- c(x$data, symbolize(dat))
      if (has_group) x$data <- c(x$data, traceify(dat, "group"))
      if (!has_color && !has_symbol && !has_group) x$data <- c(x$data, list(dat))
    } else {
      x$data <- c(x$data, list(d))
    }
  }
  # it's possible have nested layouts (e.g., plot_ly() %>% layout() %>% layout())
  nms <- names(l$layout)
  idx <- nms %in% "layout"
  l$layout <- c(list(l$layout[!idx]), setNames(l$layout[idx], NULL))
  for (i in seq_along(l$layout)) {
    x$layout[[i]] <- perform_eval(l$layout[[i]])
  }
  x$layout <- Reduce(modifyList, x$layout)
  # if style is not null, use it to modify existing traces
  if (!is.null(l$style)) {
    for (i in seq_along(l$style)) {
      sty <- perform_eval(l$style[[i]])
      for (k in l$style[[i]]$traces) 
        x$data[[k]] <- modifyList(x$data[[k]], sty)
    }
  }
  # add appropriate axis title (if they don't already exist)
  x <- axis_titles(x, l)
  # tack on other keyword arguments, if necessary
  idx <- !names(l) %in% c("data", "layout")
  if (any(idx)) x <- c(x, l[idx])
  x <- add_boxed(x)
  # ugh, annotations _must_ be an _array_ of object(s)...
  a <- x$layout$annotations
  if (!is.null(a) && !is.null(names(a))) {
    x$layout$annotations <- list(x$layout$annotations)
  }
  # traces shouldn't have any names
  x$data <- setNames(x$data, NULL)
  # if this is a non-line scatter trace and no hovermode exists, 
  # set hovermode to closest
  if (is.null(x$data[[1]]$type) || isTRUE(x$data[[1]]$type == "scatter")) {
    if (!grepl("lines", x$data[[1]]$mode %||% "lines"))
      x$layout$hovermode <- x$layout$hovermode %||% "closest"
  }
  # add plotly class mainly for printing method
  structure(x, class = unique("plotly", class(x)))
}

# returns a _list of traces_.
colorize <- function(dat, title = "") {
  cols <- dat[["color"]] %||% dat[["z"]]
  if (is.numeric(cols)) {
    # by default, use viridis::viridis(10) -> http://rud.is/b/2015/07/20/using-the-new-viridis-colormap-in-r-thanks-to-simon-garnier/
    colors <- dat[["colors"]] %||% viridis::viridis(10)
    cols <- as.vector(cols)
    rng <- range(cols, na.rm = TRUE)
    x <- seq(min(rng), max(rng), length.out = 10)
    colz <- scales::col_numeric(colors, rng, na.color = "transparent")(x)
    df <- if (length(cols) > 1) data.frame(scales::rescale(x), colz) 
    else data.frame(c(0, 1), rep(colz, 2))
    col_list <- list(
      colorbar = list(title = as.character(title)),
      colorscale = setNames(df, NULL)
    )
    # scatter-like traces can have both line and marker objects
    if (grepl("scatter", dat[["type"]] %||% "scatter")) {
      col_list$color <- cols
      dat[["marker"]] <- modifyList(col_list, dat[["marker"]] %||% list())
      #mode <- dat[["mode"]] %||% "markers+lines"
      # can't have a colorscale for both markers and lines???
      #dat[["line"]] <- modifyList(col_list, dat[["line"]] %||% list())
    } else {
      dat <- c(dat, col_list)
    }
    dat <- list(dat)
  } else { # discrete color scale
    lvls <- unique(cols)
    N <- length(lvls)
    default <- if (is.ordered(cols)) viridis::viridis(N) 
    else RColorBrewer::brewer.pal(N, "Set2")
    colors <- dat[["colors"]] %||% default
    colz <- scales::col_factor(colors, levels = lvls, na.color = "transparent")(lvls)
    dat <- traceify(dat, "color")
    dat <- Map(function(x, y) { x[["marker"]] <- c(x[["marker"]], list(color = y)); x }, 
               dat, colz)
  }
  dat <- lapply(dat, function(x) { x$color <- NULL; x$colors <- NULL; x })
  dat
}

symbolize <- function(dat) {
  # symbols really only make sense when markers are in the mode, right?
  dat$mode <- dat$mode %||% "markers"
  dat <- traceify(dat, "symbol")
  dat <- lapply(dat, function(x) { x$symbol <- NULL; x })
  N <- length(dat)
  if (N > 8) warning("Plotly supports 8 different symbols, but you have ", N, " levels!")
  symbols <- c('dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up')
  sym <- dat[[1]][["symbols"]][seq_len(N)] %||% symbols[seq_len(N)]
  dat <- Map(function(x, y) { x$marker$symbol <- y; x }, dat, sym)
  dat
}

# break up a single trace into multiple traces according to values stored 
# a particular key name
traceify <- function(dat, nm = "group") {
  x <- dat[[nm]]
  if (is.null(x)) {
    return(list(dat))
  } else {
    # the order of lvls determines the order in which traces are drawn
    # for ordered factors at least, it makes sense to draw the highest level first
    # since that _should_ be the darkest color in a sequential pallette
    lvls <- if (is.factor(x)) rev(levels(x)) else unique(x)
    n <- length(x)
    # recursively search for a non-list of appropriate length (if it is, subset it)
    recurse <- function(z, n, idx) {
      if (is.list(z)) lapply(z, recurse, n, idx) else if (length(z) == n) z[idx] else z
    }
    new_dat <- list()
    for (j in seq_along(lvls)) {
      new_dat[[j]] <- lapply(dat, function(y) recurse(y, n, x %in% lvls[j]))
      new_dat[[j]]$name <- lvls[j]
    }
    return(new_dat)
  }
}

axis_titles <- function(x, l) {
  d <- l$data[[1]]
  argz <- as.list(d$args)
  scene <- if (isTRUE(d$type %in% c("scatter3d", "surface"))) TRUE else FALSE
  for (i in c("x", "y", "z")) {
    ax <- paste0(i, "axis")
    t <- x$layout[[ax]]$title %||% x$layout$scene[[ax]]$title
    if (is.null(t)) {
      idx <- which(names(argz) %in% i)
      if (length(idx)) {
        title <- if (is.language(argz[[idx]])) deparse(argz[[idx]]) else i
        if (scene) x$layout[["scene"]][[ax]]$title <- title 
        else x$layout[[ax]]$title <- title
      }
    }
  }
  x
}

#' Create a complete empty plotly graph.
#' 
#' Useful when used with \link{subplot}
#' 
#' @export
plotly_empty <- function() {
  eaxis <- list(
    showticklabels = FALSE,
    showgrid = FALSE,
    zeroline = FALSE
  )
  layout(plot_ly(), xaxis = eaxis, yaxis = eaxis)
}
