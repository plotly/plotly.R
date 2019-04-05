#' Initiate a plotly visualization
#'
#' This function maps R objects to [plotly.js](https://plot.ly/javascript/),
#' an (MIT licensed) web-based interactive charting library. It provides 
#' abstractions for doing common things (e.g. mapping data values to 
#' fill colors (via `color`) or creating [animation]s (via `frame`)) and sets
#' some different defaults to make the interface feel more 'R-like' 
#' (i.e., closer to [plot()] and [ggplot2::qplot()]). 
#'
#' @details Unless `type` is specified, this function just initiates a plotly 
#' object with 'global' attributes that are passed onto downstream uses of
#' [add_trace()] (or similar). A [formula] must always be used when 
#' referencing column name(s) in `data` (e.g. `plot_ly(mtcars, x = ~wt)`).
#' Formulas are optional when supplying values directly, but they do
#' help inform default axis/scale titles
#' (e.g., `plot_ly(x = mtcars$wt)` vs `plot_ly(x = ~mtcars$wt)`)
#'
#' @param data A data frame (optional) or [crosstalk::SharedData] object.
#' @param ... Arguments (i.e., attributes) passed along to the trace `type`.
#' See [schema()] for a list of acceptable attributes for a given trace `type`
#' (by going to `traces` -> `type` -> `attributes`). Note that attributes
#' provided at this level may override other arguments 
#' (e.g. `plot_ly(x = 1:10, y = 1:10, color = I("red"), marker = list(color = "blue"))`).
#' @param type A character string specifying the trace type (e.g. `"scatter"`, `"bar"`, `"box"`, etc).
#' If specified, it *always* creates a trace, otherwise 
#' @param name Values mapped to the trace's name attribute. Since a trace can 
#' only have one name, this argument acts very much like `split` in that it 
#' creates one trace for every unique value.
#' @param color Values mapped to relevant 'fill-color' attribute(s) 
#' (e.g. [fillcolor](https://plot.ly/r/reference#scatter-fillcolor), 
#' [marker.color](https://plot.ly/r/reference#scatter-marker-color), 
#' [textfont.color](https://plot.ly/r/reference/#scatter-textfont-color), etc.).
#' The mapping from data values to color codes may be controlled using
#' `colors` and `alpha`, or avoided altogether via [I()] (e.g., `color = I("red")`). 
#' Any color understood by [grDevices::col2rgb()] may be used in this way. 
#' @param colors Either a colorbrewer2.org palette name (e.g. "YlOrRd" or "Blues"), 
#' or a vector of colors to interpolate in hexadecimal "#RRGGBB" format, 
#' or a color interpolation function like `colorRamp()`.
#' @param stroke Similar to `color`, but values are mapped to relevant 'stroke-color' attribute(s)
#' (e.g., [marker.line.color](https://plot.ly/r/reference#scatter-marker-line-color)
#'  and [line.color](https://plot.ly/r/reference#scatter-line-color)
#' for filled polygons). If not specified, `stroke` inherits from `color`.
#' @param strokes Similar to `colors`, but controls the `stroke` mapping.
#' @param alpha A number between 0 and 1 specifying the alpha channel applied to `color`.
#' Defaults to 0.5 when mapping to [fillcolor](https://plot.ly/r/reference#scatter-fillcolor) and 1 otherwise.
#' @param alpha_stroke Similar to `alpha`, but applied to `stroke`.
#' @param symbol (Discrete) values mapped to [marker.symbol](https://plot.ly/r/reference#scatter-marker-symbol).
#' The mapping from data values to symbols may be controlled using
#' `symbols`, or avoided altogether via [I()] (e.g., `symbol = I("pentagon")`). 
#' Any [pch] value or [symbol name](https://plot.ly/r/reference#scatter-marker-symbol) may be used in this way.
#' @param symbols A character vector of [pch] values or [symbol names](https://plot.ly/r/reference#scatter-marker-symbol).
#' @param linetype (Discrete) values mapped to [line.dash](https://plot.ly/r/reference#scatter-line-dash).
#' The mapping from data values to symbols may be controlled using
#' `linetypes`, or avoided altogether via [I()] (e.g., `linetype = I("dash")`). 
#' Any `lty` (see [par]) value or [dash name](https://plot.ly/r/reference#scatter-line-dash) may be used in this way.
#' @param linetypes A character vector of `lty` values or [dash names](https://plot.ly/r/reference#scatter-line-dash)
#' @param size (Numeric) values mapped to relevant 'fill-size' attribute(s) 
#' (e.g., [marker.size](https://plot.ly/r/reference#scatter-marker-size), 
#' [textfont.size](https://plot.ly/r/reference#scatter-textfont-size),
#' and [error_x.width](https://plot.ly/r/reference#scatter-error_x-width)).
#' The mapping from data values to symbols may be controlled using
#' `sizes`, or avoided altogether via [I()] (e.g., `size = I(30)`). 
#' @param sizes A numeric vector of length 2 used to scale `size` to pixels.
#' @param span (Numeric) values mapped to relevant 'stroke-size' attribute(s) 
#' (e.g., 
#' [marker.line.width](https://plot.ly/r/reference#scatter-marker-line-width),
#' [line.width](https://plot.ly/r/reference#scatter-line-width) for filled polygons,
#' and [error_x.thickness](https://plot.ly/r/reference#scatter-error_x-thickness))
#' The mapping from data values to symbols may be controlled using
#' `spans`, or avoided altogether via [I()] (e.g., `span = I(30)`). 
#' @param spans A numeric vector of length 2 used to scale `span` to pixels.
#' @param split (Discrete) values used to create multiple traces (one trace per value).
#' @param frame (Discrete) values used to create animation frames.
#' @param width	Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @param source a character string of length 1. Match the value of this string 
#' with the source argument in [event_data()] to retrieve the 
#' event data corresponding to a specific plot (shiny apps can have multiple plots).
#' @author Carson Sievert
#' @references <https://plotly-r.com/overview.html>
#' @seealso \itemize{
#'  \item For initializing a plotly-geo object: [plot_geo()]
#'  \item For initializing a plotly-mapbox object: [plot_mapbox()]
#'  \item For translating a ggplot2 object to a plotly object: [ggplotly()]
#'  \item For modifying any plotly object: [layout()], [add_trace()], [style()]
#'  \item For linked brushing: [highlight()]
#'  \item For arranging multiple plots: [subplot()], [crosstalk::bscols()]
#'  \item For inspecting plotly objects: [plotly_json()]
#'  \item For quick, accurate, and searchable plotly.js reference: [schema()]
#' }
#' @export
#' @examples
#' \dontrun{
#' 
#' # plot_ly() tries to create a sensible plot based on the information you 
#' # give it. If you don't provide a trace type, plot_ly() will infer one.
#' plot_ly(economics, x = ~pop)
#' plot_ly(economics, x = ~date, y = ~pop)
#' # plot_ly() doesn't require data frame(s), which allows one to take 
#' # advantage of trace type(s) designed specifically for numeric matrices
#' plot_ly(z = ~volcano)
#' plot_ly(z = ~volcano, type = "surface")
#' 
#' # plotly has a functional interface: every plotly function takes a plotly
#' # object as it's first input argument and returns a modified plotly object
#' add_lines(plot_ly(economics, x = ~date, y = ~unemploy/pop))
#' 
#' # To make code more readable, plotly imports the pipe operator from magrittr
#' economics %>% plot_ly(x = ~date, y = ~unemploy/pop) %>% add_lines()
#' 
#' # Attributes defined via plot_ly() set 'global' attributes that 
#' # are carried onto subsequent traces, but those may be over-written
#' plot_ly(economics, x = ~date, color = I("black")) %>%
#'  add_lines(y = ~uempmed) %>%
#'  add_lines(y = ~psavert, color = I("red"))
#' 
#' # Attributes are documented in the figure reference -> https://plot.ly/r/reference
#' # You might notice plot_ly() has named arguments that aren't in this figure
#' # reference. These arguments make it easier to map abstract data values to
#' # visual attributes.
#' p <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length) 
#' add_markers(p, color = ~Petal.Length, size = ~Petal.Length)
#' add_markers(p, color = ~Species)
#' add_markers(p, color = ~Species, colors = "Set1")
#' add_markers(p, symbol = ~Species)
#' add_paths(p, linetype = ~Species)
#' 
#' }
#' 
plot_ly <- function(data = data.frame(), ..., type = NULL, name,
                    color, colors = NULL, alpha = NULL, 
                    stroke, strokes = NULL, alpha_stroke = 1,
                    size, sizes = c(10, 100), 
                    span, spans = c(1, 20),
                    symbol, symbols = NULL, 
                    linetype, linetypes = NULL,
                    split, frame, 
                    width = NULL, height = NULL, source = "A") {
  
  if (!is.data.frame(data) && !crosstalk::is.SharedData(data)) {
    stop("First argument, `data`, must be a data frame or shared data.", call. = FALSE)
  }
  
  # "native" plotly arguments
  attrs <- list(...)
  
  # warn about old arguments that are no longer supported
  for (i in c("filename", "fileopt", "world_readable")) {
    if (is.null(attrs[[i]])) next
    warning("Ignoring ", i, ". Use `plotly_POST()` if you want to post figures to plotly.")
    attrs[[i]] <- NULL
  }
  if (!is.null(attrs[["group"]])) {
    warning(
      "The group argument has been deprecated. Use `group_by()` or split instead.\n",
      "See `help('plotly_data')` for examples"
    )
    attrs[["group"]] <- NULL
  }
  if (!is.null(attrs[["inherit"]])) {
    warning("The inherit argument has been deprecated.")
    attrs[["inherit"]] <- NULL
  }
  
  # tack on variable mappings
  attrs$name <- if (!missing(name)) name
  attrs$color <- if (!missing(color)) color
  attrs$stroke <- if (!missing(stroke)) stroke
  attrs$size <- if (!missing(size)) size
  attrs$span <- if (!missing(span)) span
  attrs$symbol <- if (!missing(symbol)) symbol
  attrs$linetype <- if (!missing(linetype)) linetype
  attrs$split <- if (!missing(split)) split
  attrs$frame <- if (!missing(frame)) frame
  
  # tack on scale ranges
  attrs$colors <- colors
  attrs$strokes <- strokes
  attrs$alpha <- alpha
  attrs$alpha_stroke <- alpha_stroke
  attrs$sizes <- sizes
  attrs$spans <- spans
  attrs$symbols <- symbols
  attrs$linetypes <- linetypes
  
  # and, of course, the trace type
  attrs$type <- type
  
  # id for tracking attribute mappings and finding the most current data
  id <- new_id()
  # avoid weird naming clashes
  plotlyVisDat <- data
  p <- list(
    visdat = setNames(list(function() plotlyVisDat), id),
    cur_data = id,
    attrs = setNames(list(attrs), id),
    # we always deal with a _list_ of traces and _list_ of layouts 
    # since they can each have different data
    layout = list(
      width = width, 
      height = height,
      # sane margin defaults (mainly for RStudio)
      margin = list(b = 40, l = 60, t = 25, r = 10)
    ),
    source = source
  )
  # ensure the collab button is shown (and the save/edit button is hidden) by default
  config(as_widget(p))
}


#' Initiate a plotly-mapbox object
#' 
#' Use this function instead of [plot_ly()] to initialize
#' a plotly-mapbox object. This enforces the entire plot so use
#' the scattermapbox trace type, and enables higher level geometries
#' like [add_polygons()] to work
#' 
#' @param data A data frame (optional).
#' @param ... arguments passed along to [plot_ly()]. They should be
#' valid scattermapbox attributes - \url{https://plot.ly/r/reference/#scattermapbox}.
#' Note that x/y can also be used in place of lat/lon.
#' @export
#' @author Carson Sievert
#' @seealso [plot_ly()], [plot_geo()], [ggplotly()] 
#' 
#' @examples \dontrun{
#' 
#' plot_mapbox(res_mn)
#' plot_mapbox(res_mn, color = ~INDRESNAME)
#' 
#' map_data("world", "canada") %>%
#'   group_by(group) %>%
#'   plot_mapbox(x = ~long, y = ~lat) %>%
#'   add_polygons() %>%
#'   layout(
#'     mapbox = list(
#'       center = list(lat = ~median(lat), lon = ~median(long))
#'     )
#'   )
#' }
#' 
plot_mapbox <- function(data = data.frame(), ...) {
  p <- config(plot_ly(data, ...), mapboxAccessToken = mapbox_token())
  # not only do we use this for is_mapbox(), but also setting the layout attr
  # https://plot.ly/r/reference/#layout-mapbox
  p$x$layout$mapType <- "mapbox"
  geo2cartesian(p)
}

#' Initiate a plotly-geo object
#' 
#' Use this function instead of [plot_ly()] to initialize
#' a plotly-geo object. This enforces the entire plot so use
#' the scattergeo trace type, and enables higher level geometries
#' like [add_polygons()] to work
#' 
#' @param data A data frame (optional).
#' @param ... arguments passed along to [plot_ly()].
#' @param offline whether or not to include geo assets so that the map
#' can be viewed with or without an internet connection. The plotlyGeoAssets
#' package is required for this functionality.
#' @export
#' @author Carson Sievert
#' @seealso [plot_ly()], [plot_mapbox()], [ggplotly()] 
#' @examples
#' 
#' map_data("world", "canada") %>%
#'   group_by(group) %>%
#'   plot_geo(x = ~long, y = ~lat) %>%
#'   add_markers(size = I(1))
#' 
plot_geo <- function(data = data.frame(), ..., offline = FALSE) {
  p <- plot_ly(data, ...)
  
  if (isTRUE(offline)) {
    if (system.file(package = "plotlyGeoAssets") == "") {
      stop(
        "The plotlyGeoAssets package is required to make 'offline' maps. ",
        "Please install and try again.",
        call. = FALSE
      )
    }
    p$dependencies <- c(
      list(plotlyGeoAssets::geo_assets()),
      p$dependencies
    )
  }
  # not only do we use this for is_geo(), but also setting the layout attr
  # https://plot.ly/r/reference/#layout-geo
  p$x$layout$mapType <- "geo"
  geo2cartesian(p)
}


#' Plot an interactive dendrogram
#' 
#' This function takes advantage of nested key selections to implement an 
#' interactive dendrogram. Selecting a node selects all the labels (i.e. leafs)
#' under that node.
#' 
#' @param d a dendrogram object
#' @param set defines a crosstalk group
#' @param xmin minimum of the range of the x-scale
#' @param width width
#' @param height height
#' @param ... arguments supplied to [subplot()]
#' @export
#' @author Carson Sievert
#' @seealso [plot_ly()], [plot_mapbox()], [ggplotly()] 
#' @examples
#' 
#' hc <- hclust(dist(USArrests), "ave")
#' dend1 <- as.dendrogram(hc)
#' plot_dendro(dend1, height = 600) %>% 
#'   hide_legend() %>% 
#'   highlight(persistent = TRUE, dynamic = TRUE)
#' 

plot_dendro <- function(d, set = "A", xmin = -50, height = 500, width = 500, ...) {
  # get x/y locations of every node in the tree
  allXY <- get_xy(d)
  # get non-zero heights so we can split on them and find the relevant labels
  non0 <- allXY[["y"]][allXY[["y"]] > 0]
  # splitting on the minimum height would generate all terminal nodes anyway
  split <- non0[min(non0) < non0]
  # label is a list-column since non-zero heights have multiple labels
  # for now, we just have access to terminal node labels
  labs <- labels(d)
  allXY$label <- vector("list", nrow(allXY))
  allXY$label[[1]] <- labs
  allXY$label[allXY$y == 0] <- labs
  
  # collect all the *unique* non-trivial nodes
  nodes <- list()
  for (i in split) {
    dsub <- cut(d, i)$lower
    for (j in seq_along(dsub)) {
      s <- dsub[[j]]
      if (is.leaf(s)) next
      if (any(vapply(nodes, function(x) identical(x, s), logical(1)))) next
      nodes[[length(nodes) + 1]] <- s
    }
  }
  
  heights <- sapply(nodes, function(x) attr(x, "height"))
  labs <- lapply(nodes, labels)
  
  # NOTE: this won't support nodes that have the same height 
  # but that isn't possible, right?
  for (i in seq_along(heights)) {
    allXY$label[[which(allXY$y == heights[i])]] <- labs[[i]]
  }
  
  tidy_segments <- dendextend::as.ggdend(d)$segments
  
  allTXT <- allXY[allXY$y == 0, ]
  
  blank_axis <- list(
    title = "",
    showticklabels = FALSE,
    zeroline = FALSE
  )
  
  allXY$members <- sapply(allXY$label, length)
  allTXT$label <- as.character(allTXT$label)
  
  allXY %>% 
    plot_ly(x = ~y, y = ~x, color = I("black"), hoverinfo = "none",
            height = height, width = width) %>%
    add_segments(
      data = tidy_segments, xend = ~yend, yend = ~xend, showlegend = FALSE
    ) %>%
    add_markers(
      data = allXY[allXY$y > 0, ], key = ~label, set = set, name = "nodes", 
      text = ~paste0("members: ", members), hoverinfo = "text"
    ) %>%
    add_text(
      data = allTXT, x = 0, y = ~x, text = ~label, key = ~label, set = set,
      textposition = "middle left", name = "labels"
    ) %>%
    layout(
      dragmode = "select", 
      xaxis = c(blank_axis, list(range = c(xmin, extendrange(allXY[["y"]])[2]))),
      yaxis = c(blank_axis, list(range = extendrange(allXY[["x"]])))
    )
}

get_xy <- function(node) {
  m <- dendextend::get_nodes_xy(node)
  colnames(m) <- c("x", "y")
  tibble::as_tibble(m)
}


#' Convert a list to a plotly htmlwidget object
#' 
#' @param x a plotly object.
#' @param ... other options passed onto `htmlwidgets::createWidget`
#' @export
#' @examples 
#' 
#' trace <- list(x = 1, y = 1)
#' obj <- list(data = list(trace), layout = list(title = "my plot"))
#' as_widget(obj)
#' 

as_widget <- function(x, ...) {
  if (inherits(x, "htmlwidget")) return(x)
  # add plotly class mainly for printing method
  # customize the JSON serializer (for htmlwidgets)
  attr(x, 'TOJSON_FUNC') <- to_JSON
  htmlwidgets::createWidget(
    name = "plotly",
    x = x,
    width = x$layout$width,
    height = x$layout$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = TRUE,
      defaultWidth = '100%',
      defaultHeight = 400
    ),
    preRenderHook = plotly_build,
    dependencies = c(
      list(typedArrayPolyfill()),
      crosstalk::crosstalkLibs(),
      list(plotlyHtmlwidgetsCSS()),
      list(plotlyMainBundle())
    )
  )
}

typedArrayPolyfill <- function() {
  htmltools::htmlDependency(
    name = "typedarray", 
    version = "0.1",
    package = "plotly",
    src = dependency_dir("typedarray"),
    script = "typedarray.min.js",
    all_files = FALSE
  )
}

plotlyMainBundle <- function() {
  htmltools::htmlDependency(
    name = "plotly-main", 
    version = "1.46.1",
    package = "plotly",
    src = dependency_dir("plotlyjs"),
    script = "plotly-latest.min.js",
    all_files = FALSE
  )
}

plotlyHtmlwidgetsCSS <- function() {
  htmltools::htmlDependency(
    name = "plotly-htmlwidgets-css", 
    version = plotlyMainBundle()$version,
    package = "plotly",
    src = dependency_dir("plotlyjs"),
    stylesheet = "plotly-htmlwidgets.css",
    all_files = FALSE
  )
}

locale_dependency <- function(locale) {
  if (!is.character(locale) || length(locale) != 1) {
    stop("locale must be a character string (vector of length 1)", call. = FALSE)
  }
  
  locale_dir <- dependency_dir("plotlyjs", "locales")
  locales_all <- sub("\\.js$", "", list.files(system.file(locale_dir, package = "plotly")))
  if (!tolower(locale) %in% locales_all) {
    stop(
      "Invalid locale: '", locale, "'.\n\n",
      sprintf("Supported locales include: '%s'", paste(locales_all, collapse = "', '")),
      call. = FALSE
    )
  }
  
  # some locales rely on a base/main locale (e.g. de-CH relies on de)
  # https://codepen.io/etpinard/pen/pKvLVX?editors=1010
  scripts <- paste0(locale, ".js")
  if (grepl("-", locale)) {
    locale_main <- strsplit(locale, "-")[[1]][1]
    if (locale_main %in% locales_all) {
      scripts <- c(scripts, paste0(locale_main, ".js"))
    }
  }
  
  htmltools::htmlDependency(
    name = paste0("plotly-locale-", locale),
    version = plotlyMainBundle()$version,
    package = "plotly",
    src = list(file = locale_dir),
    script = tolower(scripts),
    all_files = FALSE
  )
}

#' Remove TypedArray polyfill
#'
#' By default, plotly.js' TypedArray polyfill is included as a dependency, so
#' printing "just works" in any context. Many users won't need this polyfill,
#' so this function may be used to remove it and thus reduce the size of the page. 
#' 
#' @details The polyfill seems to be only relevant for those rendering plots 
#' via phantomjs and RStudio on some Windows platforms.
#'
#' @param p a plotly object
#' @export
#' @examples 
#' 
#' \dontrun{
#' p1 <- plot_ly()
#' p2 <- remove_typedarray_polyfill(p1)
#' t1 <- tempfile(fileext = ".html")
#' htmlwidgets::saveWidget(p1, t1)
#' file.info(t1)$size
#' htmlwidgets::saveWidget(p2, t1)
#' file.info(t1)$size
#' }

remove_typedarray_polyfill <- function(p) {
  isTA <- vapply(p$dependencies, function(x) identical(x[["name"]], "typedarray"), logical(1))
  p$dependencies <- p$dependencies[!isTA]
  p
}
