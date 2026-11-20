# Rendering plot_ly() objects with plotly.core
#
# plot_ly() objects are still built by plotly_build(), and the result is then
# converted to a plotly.core figure (as_go_figure()): every trace is added with
# plotly.core's add_<type>() and the layout with update_layout(), which
# validate them against the bundled plotly.js. The figure is rendered with
# plotly.core's widget (and its Shiny bindings and static image export).
#
# plotly.js has removed some features since plotly 4.x was written (Mapbox
# traces, heatmapgl, legacy polar charts, title aliases, transforms); plots
# using them are upgraded to their current equivalents when built.

# Schema -----------------------------------------------------------------------

# plotly.js's plot schema, as the build uses it (`Schema`): plotly.core's, with
# the removed Mapbox and WebGL heatmap types as aliases of their current
# equivalents (plots using them are built as before, then upgraded), and the
# trace transforms applied in R.
plotly_schema <- function() {
  s <- plotly.core::schema(jsonedit = FALSE)
  for (old in names(REMOVED_TYPES)) s$traces[[old]] <- s$traces[[REMOVED_TYPES[[old]]]]
  s$layout$layoutAttributes$mapbox <- s$layout$layoutAttributes$map
  s$transforms <- transforms_schema()
  s
}

# (bound when the package is loaded, see .onLoad())
utils::globalVariables("Schema")

# Upgrades ---------------------------------------------------------------------

# plotly.js 3 removed the Mapbox-based traces and layout.mapbox (replaced by
# the MapLibre-based "map" ones, which need no access token) and heatmapgl.
REMOVED_TYPES <- c(scattermapbox = "scattermap", choroplethmapbox = "choroplethmap",
                   densitymapbox = "densitymap", heatmapgl = "heatmap")

# Mapbox styles that need a Mapbox token, or are gone (Stamen), as their
# closest MapLibre style
MAPBOX_STYLES <- c("stamen-terrain" = "outdoors", "stamen-toner" = "light",
                   "stamen-watercolor" = "outdoors")

upgrade_trace <- function(tr) {
  type <- tr[["type"]]
  if (is.character(type) && length(type) == 1 && type %in% names(REMOVED_TYPES)) tr[["type"]] <- REMOVED_TYPES[[type]]
  sp <- tr[["subplot"]]
  if (is.character(sp) && length(sp) == 1) tr[["subplot"]] <- sub("^mapbox", "map", sp)
  tr
}

# layout.mapbox* as layout.map*; the bounds fit_bounds() computes become the
# figure's fitBounds setting (plotly.core's widget fits the map to them)
upgrade_map_layout <- function(layout, fit = list()) {
  for (nm in grep("^mapbox[0-9]*$", names(layout), value = TRUE)) {
    m <- layout[[nm]]
    new <- sub("^mapbox", "map", nm)
    if (is.list(m)) {
      m$accesstoken <- NULL
      st <- m$style
      if (is.character(st) && length(st) == 1) {
        if (st %in% names(MAPBOX_STYLES)) m$style <- MAPBOX_STYLES[[st]]
        else if (grepl("^mapbox://", st)) m$style <- "basic"
      }
      b <- m[["_fitBounds"]]$bounds
      if (length(b) == 4) {
        fit[[new]] <- I(b)
        m$center <- m$center %||% list(lat = mean(b[c(2, 4)]), lon = mean(b[c(1, 3)]))
      }
      m[["_fitBounds"]] <- NULL
    }
    layout[[nm]] <- NULL
    # settings given for both: the mapbox ones win
    layout[[new]] <- modify_list(layout[[new]], if (is.list(m)) m else list())
  }
  attr(layout, "fitBounds") <- if (length(fit)) fit
  layout
}

# titlefont, titleside and titleposition (aliases plotly.js 3 removed) as
# title.font, title.side and title.position, in any object
TITLE_ALIASES <- c(titlefont = "font", titleside = "side", titleposition = "position")

upgrade_titles <- function(x) {
  if (!is.list(x) || is.data.frame(x)) return(x)
  nms <- names(x)
  if (!is.null(nms) && any(names(TITLE_ALIASES) %in% nms)) {
    t <- x[["title"]]
    t <- if (is.null(t)) list() else if (is.list(t)) t else list(text = t)
    for (a in intersect(names(TITLE_ALIASES), nms)) {
      k <- TITLE_ALIASES[[a]]
      t[[k]] <- if (is.list(x[[a]]) && is.list(t[[k]])) modify_list(x[[a]], t[[k]]) else t[[k]] %||% x[[a]]
      x[[a]] <- NULL
    }
    x[["title"]] <- t
  }
  if (length(x)) x[] <- lapply(x, upgrade_titles)
  x
}

# Legacy polar charts (scatter traces with r and t, removed in plotly.js 2)
# as scatterpolar traces: angles t in degrees running clockwise from 0 at the
# right, the chart turned by layout.orientation, plot_bgcolor as background.
upgrade_legacy_polar <- function(p) {
  polar <- FALSE
  p$x$data <- lapply(p$x$data, function(tr) {
    type <- tr[["type"]] %||% "scatter"
    if (type %in% c("scatter", "scattergl") && !is.null(tr[["r"]]) && !is.null(tr[["t"]])) {
      polar <<- TRUE
      tr[["type"]] <- "scatterpolar"
      tr[["theta"]] <- tr[["t"]]
      tr[c("t", "x", "y", "xaxis", "yaxis", "error_x", "error_y")] <- NULL
    }
    tr
  })
  if (!polar) return(p)
  l <- p$x$layout
  pol <- l$polar %||% list()
  pol$angularaxis <- modify_list(list(direction = "clockwise", rotation = -(l$orientation %||% 0)),
                                 modify_list(l$angularaxis, pol$angularaxis))
  pol$radialaxis <- modify_list(l$radialaxis, pol$radialaxis)
  if (!length(pol$radialaxis)) pol$radialaxis <- NULL
  if (!is.null(l$plot_bgcolor) && is.null(pol$bgcolor)) pol$bgcolor <- l$plot_bgcolor
  l[c("orientation", "angularaxis", "radialaxis")] <- NULL
  l$polar <- pol
  p$x$layout <- l
  p
}

# Traces and layout upgraded to what plotly.js supports now (called by
# plotly_build() before it checks attribute names).
upgrade_removed <- function(p) {
  # as plotly 4.x did, warn about a top-level titlefont (which is upgraded)
  if ("titlefont" %in% c(names(p$x$layout), unlist(lapply(p$x$data, names)))) {
    warning("The titlefont attribute is deprecated. Use title = list(font = ...) instead.", call. = FALSE)
  }
  p$x$data <- upgrade_titles(lapply(p$x$data, upgrade_trace))
  p$x$layout <- upgrade_titles(upgrade_map_layout(p$x$layout))
  p$x$fitBounds <- attr(p$x$layout, "fitBounds")
  attr(p$x$layout, "fitBounds") <- NULL
  if (length(p$x$frames)) {
    p$x$frames <- lapply(p$x$frames, function(f) {
      if (is.list(f) && length(f$data)) f$data <- upgrade_titles(lapply(f$data, upgrade_trace))
      if (is.list(f) && length(f$layout)) {
        f$layout <- upgrade_titles(upgrade_map_layout(f$layout))
        attr(f$layout, "fitBounds") <- NULL
      }
      f
    })
  }
  p$x$config$mapboxAccessToken <- NULL
  p
}

# Conversion to a plotly.core figure ---------------------------------------------

#' Convert a plotly object to a plotly.core figure
#'
#' Builds a [plot_ly()] (or [ggplotly()]) object with [plotly_build()] and
#' returns the equivalent [plotly.core::go_figure()]: each trace is added with
#' plotly.core's `add_<type>()` function (e.g. [plotly.core::add_scatter()])
#' and the layout with [plotly.core::update_layout()], which validate them
#' against plotly.js. This is how plotly objects are rendered. Attributes that
#' are not valid are dropped with a warning.
#'
#' @param p a plotly object.
#' @return A [plotly.core::go_figure()], carrying the plot's config, Shiny
#'   event and linked-highlighting settings.
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' fig <- as_go_figure(plot_ly(mtcars, x = ~wt, y = ~mpg))
#' plotly.core::update_traces(fig, marker_size = 10)
as_go_figure <- function(p) {
  b <- plotly_build(p)$x
  # values the build supplies by default for attributes a trace type doesn't
  # have (e.g. marker.line of a scattermap trace), which plotly.js ignored
  b$data <- lapply(b$data, function(tr) drop_generated(tr, tr[["type"]] %||% "scatter"))
  x <- extract_keys(strip_internal(b))
  # ggplotly()'s internal marker for axis-title annotations (see subplot())
  x$layout$annotations <- lapply(x$layout$annotations, function(a) { a$annotationType <- NULL; a })
  x$layout$mapType <- NULL
  # subplot() of plots without axes (tables, images, ...) gives "NA" axis and
  # subplot ids, which plotly.js ignored
  x$layout <- x$layout[!grepl("^NA[0-9]*$", names(x$layout))]
  x$data <- lapply(x$data, function(tr) {
    for (k in c("xaxis", "yaxis", "subplot", "geo")) if (identical(grepl("^NA[0-9]*$", tr[[k]]), TRUE)) tr[[k]] <- NULL
    tr
  })
  # ggplot2 themes give font families of "" (the default font)
  x[c("data", "layout", "frames")] <- drop_blank_family(x[c("data", "layout", "frames")])
  x[c("data", "frames")] <- matrix_colorscales(x[c("data", "frames")])
  # plotly 4.x plots have no template
  if (is.null(x$layout$template)) x$layout$template <- plotly.core::get_template("none")
  if (!is.null(x$config$locale)) x$config$locale <- tolower(x$config$locale)
  # config options plotly.js doesn't have (config() stores them as given)
  bad <- setdiff(names(x$config), names(Schema$config))
  if (length(bad)) {
    warning("Invalid config option(s), ignored: '", paste(bad, collapse = "', '"), "'\n",
            "Valid options include: '", paste(names(Schema$config), collapse = "', '"), "'", call. = FALSE)
    x$config <- x$config[setdiff(names(x$config), bad)]
  }

  fig <- validated(plotly.core::go_figure(frames = x$frames), plotly.core::go_figure(frames = x$frames, skip_invalid = TRUE))
  for (tr in x$data) {
    add <- getExportedValue("plotly.core", paste0("add_", tr[["type"]] %||% "scatter"))
    args <- c(list(fig), tr[names(tr) != "type"])
    fig <- validated(do.call(add, args), do.call(add, c(args, list(skip_invalid = TRUE))))
  }
  layout <- x$layout[!vapply(x$layout, is.null, logical(1))]
  fig <- validated(plotly.core::update_layout(fig, layout),
                   plotly.core::update_layout(fig, plotly.core::go_figure(layout = layout, skip_invalid = TRUE)$layout))
  for (k in c("config", "source", "shinyEvents", "mathjax", "keys", "sets", "frameKeys", "highlight", "selectize", "fitBounds", "bundle")) {
    if (!is.null(x[[k]])) attr(fig, paste0("plotly_", k)) <- x[[k]]
  }
  fig
}

# `expr`, or (with a warning, as plotly 4.x warned about invalid attributes)
# `fallback`, which skips invalid attributes
validated <- function(expr, fallback) {
  tryCatch(expr, error = function(e) {
    warning(conditionMessage(e), "\n(invalid attributes are ignored)", call. = FALSE)
    fallback
  })
}

drop_generated <- function(x, key) {
  for (a in names(x)) {
    k <- paste(key, a, sep = ".")
    entry <- plotly.core::get_validator(k)
    if (is.null(entry)) {
      if (all_generated(x[[a]])) x[[a]] <- NULL
    } else if (is.list(x[[a]]) && !is.data.frame(x[[a]]) && !is.null(entry$children)) {
      x[[a]] <- drop_generated(x[[a]], k)
    }
  }
  x
}

all_generated <- function(x) {
  if (inherits(x, c("plotly_default", "colorway"))) return(TRUE)
  is.list(x) && length(x) > 0 && all(vapply(x, all_generated, logical(1)))
}

# The build's internal classes (defaults, colorway markers, trace classes)
# and fields that aren't plotly.js attributes
strip_internal <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    x <- unclass(x)
    attr(x, "titles") <- NULL
    for (k in c("frame", "transforms", "_isGraticule", ".plotlyVariableMapping")) x[[k]] <- NULL
    x[] <- lapply(x, strip_internal)
    # NULL and empty attribute values (plotly.js ignored the nulls and empty
    # objects they became)
    if (!is.null(names(x))) x <- x[!vapply(x, function(v) is.null(v) || (is.list(v) && !is.data.frame(v) && !length(v)), logical(1))]
    return(x)
  }
  if (!is.null(oldClass(x))) {
    keep <- setdiff(oldClass(x), c("plotly_default", "colorway", "plotly_eval", "aniSlider", "aniButton", "TeX",
                                   "numeric", "integer", "character", "logical", "double", "list",
                                   # ggplot2's vctrs classes for scaled aesthetics (e.g. bar widths)
                                   "mapped_discrete", "mapped_continuous", "vctrs_vctr"))
    oldClass(x) <- if (length(keep)) keep else NULL
  }
  x
}

# Keys and sets (linked highlighting) move from the traces to widget settings;
# a simple key with several values (the whole trace stands for them) is sent
# as {simple: [...]}.
extract_keys <- function(x) {
  take <- function(traces, what) {
    v <- lapply(traces, `[[`, what)
    if (all(vapply(v, is.null, logical(1)))) NULL else v
  }
  take_keys <- function(traces) {
    v <- lapply(traces, function(tr) {
      k <- tr[["key"]]
      if (!is.null(k) && isTRUE(tr[["_isSimpleKey"]]) && length(k) > 1) list(simple = I(as.character(k)))
      else if (length(k) == 1 && !is.list(k)) as.character(k)
      else k
    })
    if (all(vapply(v, is.null, logical(1)))) NULL else v
  }
  drop <- function(traces) lapply(traces, function(tr) {
    tr$key <- NULL; tr$set <- NULL; tr[["_bbox"]] <- NULL
    tr[["_isSimpleKey"]] <- NULL; tr[["_isNestedKey"]] <- NULL
    tr
  })
  x$keys <- take_keys(x$data)
  x$sets <- take(x$data, "set")
  x$data <- drop(x$data)
  if (length(x$frames)) {
    fk <- lapply(x$frames, function(f) list(keys = take_keys(f$data), sets = take(f$data, "set")))
    if (any(vapply(fk, function(f) !is.null(f$keys) || !is.null(f$sets), logical(1)))) x$frameKeys <- fk
    x$frames <- lapply(x$frames, function(f) { f$data <- drop(f$data); f })
  }
  if (is.null(x$sets)) {
    x$highlight <- NULL
    x$selectize <- NULL
  }
  x
}

# colorscales given as (value, color) matrices, as lists of pairs
matrix_colorscales <- function(x) {
  if (!is.list(x)) return(x)
  cs <- x[["colorscale"]]
  if (is.matrix(cs) && ncol(cs) == 2) {
    x[["colorscale"]] <- Map(function(v, c) list(v, c), as.numeric(cs[, 1]), as.character(cs[, 2]))
  }
  x[] <- lapply(x, matrix_colorscales)
  x
}

drop_blank_family <- function(x) {
  if (!is.list(x)) return(x)
  if (identical(x[["family"]], "")) x[["family"]] <- NULL
  # ggplotly() gives the fonts of blank theme elements size 0 (invalid, unused)
  fonts <- grepl("font$", names(x) %||% character())
  x[fonts] <- lapply(x[fonts], function(f) {
    if (is.list(f) && isTRUE(f[["size"]] == 0)) f[["size"]] <- NULL
    f
  })
  x[] <- lapply(x, drop_blank_family)
  x
}

# Rendering --------------------------------------------------------------------

# preRenderHook of plot_ly() widgets: the plotly.core widget's payload.
# (htmlwidgets collects dependencies before this runs, so plotly.js is
# attached when the widget is created, and replaced by partial_bundle().)
render_go_figure <- function(w) {
  w$x <- plotly.core::plotly_widget(as_go_figure(w))$x
  w
}

# partial_bundle() for plot_ly() objects: the plot is built to find its trace
# types, and its plotly.js dependency replaced (so call it last, as with
# plotly 4.x)
#' @export
partial_bundle.plotly <- function(p, type = "auto", local = TRUE, minified = TRUE) {
  w <- plotly.core::plotly_widget(as_go_figure(p), bundle = type)
  dep <- Filter(function(d) identical(d$name, "plotly-main"), w$dependencies)
  keep <- !vapply(p$dependencies, function(d) identical(d$name, "plotly-main"), logical(1))
  p$dependencies <- c(dep, p$dependencies[keep])
  # (and for the plotly.core widget the plot renders as)
  p$x$bundle <- type
  p
}

#' @export
as_widget.plotly <- function(x, ...) {
  w <- plotly.core::plotly_widget(as_go_figure(x), ...)
  w$width <- x$width %||% w$width
  w$height <- x$height %||% w$height
  w
}

# Printing, knitting and including a plot_ly() object in a page render it as
# plotly.core's widget, with all the dependencies it needs (htmlwidgets
# would otherwise only see those attached to the plot_ly() widget)
#' @export
print.plotly <- function(x, ...) {
  print(as_widget(x), ...)
  invisible(x)
}

#' @exportS3Method knitr::knit_print
knit_print.plotly <- function(x, ..., options = NULL) {
  knitr::knit_print(as_widget(x), ..., options = options)
}

#' @exportS3Method htmltools::as.tags
as.tags.plotly <- function(x, ...) htmltools::as.tags(as_widget(x), ...)

#' @export
as_widget.gg <- function(x, ...) as_widget(ggplotly(x), ...)

#' @export
as_widget.ggmatrix <- function(x, ...) as_widget(ggplotly(x), ...)

# a plotly.core figure (e.g. given to subplot()) builds like a plain list
#' @export
plotly_build.go_figure <- function(p, registerFrames = TRUE) {
  plain <- function(x) if (is.list(x)) lapply(unclass(x), plain) else x
  x <- plain(unclass(p)[c("data", "layout", if (length(p$frames)) "frames")])
  # use plot_ly()'s default margins, not plotly.js's
  x$layout$margin <- x$layout$margin %||% list(b = 40, l = 60, t = 25, r = 10)
  # settings (config, source, ...) are plotly_<name> attributes; not the data
  for (a in setdiff(grep("^plotly_", names(attributes(p)), value = TRUE), "plotly_data")) {
    x[[sub("^plotly_", "", a)]] <- attr(p, a)
  }
  x$source <- x$source %||% "A"
  w <- as_widget(x)
  # plot_ly()'s default config (unless the figure has its own)
  if (is.null(x$config)) w <- config(w)
  plotly_build(w, registerFrames)
}

# plotly.core's add_<type>() for plot_ly() objects -------------------------------

# For every trace type without its own method (add_histogram(), add_pie(),
# ... in R/add.R), plotly.core's add_<type>() adds a layer of that type to a
# plot_ly() object, evaluated when the plot is built (like add_trace()).
local({
  ns <- topenv()
  for (type in plotly.core::available_trace_types()) {
    name <- paste0("add_", type, ".plotly")
    if (exists(name, envir = ns, inherits = FALSE)) next
    assign(name, local({
      trace_type <- type
      function(fig, ..., type = NULL) add_trace.plotly(fig, ..., type = trace_type)
    }), envir = ns)
  }
})
