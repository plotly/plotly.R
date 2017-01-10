#' Convert ggplot2 to plotly
#'
#' This function converts a \code{\link[ggplot2]{ggplot}()} object to a 
#' plotly object. 
#' 
#' @details Conversion of relative sizes depends on the size of the current 
#' graphics device (if no device is open, width/height of a new (off-screen) 
#' device defaults to 640/480). In other words, \code{height} and
#' \code{width} must be specified at runtime to ensure sizing is correct.
#'
#' @param p a ggplot object.
#' @param width Width of the plot in pixels (optional, defaults to automatic sizing).
#' @param height Height of the plot in pixels (optional, defaults to automatic sizing).
#' @param tooltip a character vector specifying which aesthetic mappings to show
#' in the tooltip. The default, "all", means show all the aesthetic mappings
#' (including the unofficial "text" aesthetic). The order of variables here will
#' also control the order they appear. For example, use
#' \code{tooltip = c("y", "x", "colour")} if you want y first, x second, and
#' colour last.
#' @param dynamicTicks should plotly.js dynamically generate axis tick labels? 
#' Dynamic ticks are useful for updating ticks in response to zoom/pan
#' interactions; however, they can not always reproduce labels as they 
#' would appear in the static ggplot2 image.
#' @param layerData data from which layer should be returned?
#' @param originalData should the "original" or "scaled" data be returned?
#' @param source a character string of length 1. Match the value of this string 
#' with the source argument in \code{\link{event_data}()} to retrieve the 
#' event data corresponding to a specific plot (shiny apps can have multiple plots).
#' @param ... arguments passed onto methods.
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/ggplot2}
#' @seealso \code{\link{plot_ly}()}
#' @examples \dontrun{
#' # simple example
#' ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)
#' ggplotly(ggiris)
#'
#' data(canada.cities, package = "maps")
#' viz <- ggplot(canada.cities, aes(long, lat)) +
#'   borders(regions = "canada") +
#'   coord_equal() +
#'   geom_point(aes(text = name, size = pop), colour = "red", alpha = 1/2)
#' ggplotly(viz, tooltip = c("text", "size"))
#' 
#' 
#' # highlighting lines
#' demo("highlight-ggplotly", package = "plotly")
#' 
#' # client-side linked brushing
#' library(crosstalk)
#' d <- SharedData$new(mtcars)
#' subplot(
#'  qplot(data = d, x = mpg, y = wt),
#'  qplot(data = d, x = mpg, y = vs)
#' )
#' 
#' # client-side linked brushing in a scatterplot matrix
#' SharedData$new(iris) %>%
#'   GGally::ggpairs(aes(colour = Species), columns = 1:4) %>%
#'   ggplotly(tooltip = c("x", "y", "colour"))
#' }
#'
ggplotly <- function(p = ggplot2::last_plot(), width = NULL, height = NULL,
                     tooltip = "all", dynamicTicks = FALSE, 
                     layerData = 1, originalData = TRUE, source = "A", ...) {
  UseMethod("ggplotly", p)
}

#' @export
ggplotly.plotly <- function(p = ggplot2::last_plot(), width = NULL, height = NULL,
                            tooltip = "all", dynamicTicks = FALSE, 
                            layerData = 1, originalData = TRUE, source = "A", ...) {
  p
}

#' @export
ggplotly.ggmatrix <- function(p = ggplot2::last_plot(), width = NULL,
                              height = NULL, tooltip = "all", dynamicTicks = FALSE, 
                              layerData = 1, originalData = TRUE, source = "A", ...) {
  dots <- list(...)
  # provide a sensible crosstalk if none is already provided (makes ggnostic() work at least)
  if (!crosstalk_key() %in% names(p$data)) {
    p$data[[crosstalk_key()]] <- p$data[[".rownames"]] %||% seq_len(nrow(p$data))
    attr(p$data, "set") <- dots[["set"]] %||% new_id()
  }
  subplotList <- list()
  for (i in seq_len(p$ncol)) {
    columnList <- list()
    for (j in seq_len(p$nrow)) {
      thisPlot <- p[j, i]
      if (i == 1) {
        # should the first column contain axis labels?
        if (p$showYAxisPlotLabels %||% TRUE) thisPlot <- thisPlot + ylab(p$yAxisLabels[j])
      } else {
        # y-axes are never drawn on the interior, and diagonal plots are densities,
        # so it doesn't make sense to synch zoom actions on y
        thisPlot <- thisPlot + ylab(NULL) +
          theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
          )
      }
      columnList <- c(
        columnList, list(ggplotly(
          thisPlot, tooltip = tooltip, dynamicTicks = dynamicTicks, 
          layerData = layerData, originalData = originalData, source = source,
          width = width, height = height
        ))
      )
    }
    # conditioned on a column in a ggmatrix, the x-axis should be on the
    # same scale.
    s <- subplot(columnList, nrows = p$nrow, margin = 0.01, shareX = TRUE,
                 titleY = TRUE, titleX = TRUE)
    subplotList <- c(subplotList, list(s))
  }
  s <- subplot(subplotList, nrows = 1, margin = 0.01, 
               titleY = TRUE, titleX = TRUE) %>% 
    hide_legend() %>%
    layout(dragmode = "select")
  if (nchar(p$title %||% "") > 0) {
    s <- layout(s, title = p$title)
  }
  for (i in seq_along(p$xAxisLabels)) {
    s$x$layout[[sub("^xaxis1$", "xaxis", paste0("xaxis", i))]]$title <- p$xAxisLabels[[i]]
  }
  if (length(p$yAxisLabels)) {
    s$x$layout$margin$l <- s$x$layout$margin$l + 50
  }
  
  config(s)
}

#' @export
ggplotly.ggplot <- function(p = ggplot2::last_plot(), width = NULL,
                            height = NULL, tooltip = "all", dynamicTicks = FALSE,  
                            layerData = 1, originalData = TRUE, source = "A", ...) {
  l <- gg2list(p, width = width, height = height, tooltip = tooltip, 
               dynamicTicks = dynamicTicks, layerData = layerData, 
               originalData = originalData, source = source, ...)
  config(as_widget(l))
}

#' Convert a ggplot to a list.
#' @param p ggplot2 plot.
#' @param width Width of the plot in pixels (optional, defaults to automatic sizing).
#' @param height Height of the plot in pixels (optional, defaults to automatic sizing).
#' @param tooltip a character vector specifying which aesthetic tooltips to show in the
#' tooltip. The default, "all", means show all the aesthetic tooltips
#' (including the unofficial "text" aesthetic).
#' @param dynamicTicks should plotly.js dynamically generate axis tick labels? 
#' Dynamic ticks are useful for updating ticks in response to zoom/pan
#' interactions; however, they can not always reproduce labels as they 
#' would appear in the static ggplot2 image.
#' @param layerData data from which layer should be returned?
#' @param originalData should the "original" or "scaled" data be returned?
#' @param source a character string of length 1. Match the value of this string 
#' with the source argument in \code{\link{event_data}()} to retrieve the 
#' event data corresponding to a specific plot (shiny apps can have multiple plots).
#' @param ... currently not used
#' @return a 'built' plotly object (list with names "data" and "layout").
#' @export
gg2list <- function(p, width = NULL, height = NULL, 
                    tooltip = "all", dynamicTicks = FALSE, 
                    layerData = 1, originalData = TRUE, source = "A", ...) {
  
  # To convert relative sizes correctly, we use grid::convertHeight(),
  # which may open a new *screen* device, if none is currently open. 
  # It is undesirable to both open a *screen* device and leave a new device
  # open, so if required, we open a non-screen device now, and close on exit 
  # see https://github.com/att/rcloud.htmlwidgets/issues/2
  if (is.null(grDevices::dev.list())) {
    dev_fun <- if (system.file(package = "Cairo") != "") {
      Cairo::Cairo
    } else if (capabilities("png")) {
      grDevices::png
    } else if (capabilities("jpeg")) {
      grDevices::jpeg 
    } else {
      stop(
        "No graphics device is currently open and no cairo or bitmap device is available.\n", 
        "To ensure sizes are converted correctly, you have three options:",
        "  (1) Open a graphics device (with the desired size) b4 using ggplotly()",
        "  (2) install.packages('Cairo')",
        "  (3) compile R to use a bitmap device",
        call. = FALSE
      )
    }
    dev_fun(file = tempfile(), width = width %||% 640, height = height %||% 480)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  
  # ------------------------------------------------------------------------
  # Our internal version of ggplot2::ggplot_build(). Modified from
  # https://github.com/hadley/ggplot2/blob/0cd0ba/R/plot-build.r#L18-L92
  # ------------------------------------------------------------------------
  
  plot <- ggfun("plot_clone")(p)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$layer_data(plot$data))
  
  # save crosstalk sets before this attribute gets squashed
  sets <- lapply(layer_data, function(y) attr(y, "set"))
  
  scales <- plot$scales

  # Apply function to layer and matching data
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }
  
  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  layout <- ggfun("create_layout")(plot$facet)
  data <- layout$setup(layer_data, plot$data, plot$plot_env, plot$coordinates)
  data <- layout$map(data)
  
  # save the domain of the group for display in tooltips
  groupDomains <- Map(function(x, y) {
    tryCatch(
      eval(y$mapping[["group"]] %||% plot$mapping[["group"]], x), 
      error = function(e) NULL
    )
  }, data, layers)
  
  # for simple (StatIdentity) geoms, add crosstalk key to aes mapping
  # (effectively adding it as a group)
  # later on, for more complicated geoms (w/ non-trivial summary statistics),
  # we construct a nested key mapping (within group)
  layers <- Map(function(x, y) {
    if (crosstalk_key() %in% names(y) && inherits(x[["stat"]], "StatIdentity")) {
      x[["mapping"]] <- c(x[["mapping"]], key = as.symbol(crosstalk_key()))
    }
    x
  }, layers, layer_data)
  
  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))
  
  # add frame to group if it exists
  data <- lapply(data, function(d) { 
    if (!"frame" %in% names(d)) return(d)
    d$group <- with(d, paste(group, frame, sep = "-"))
    d
  })
  
  # The computed aesthetic codes the groups as integers
  # Here we build a map each of the integer values to the group label
  group_maps <- Map(function(x, y) {
    tryCatch({
      x_group <- x[["group"]]
      names(x_group) <- y
      x_group <- x_group[!duplicated(x_group)]
      x_group
    }, error = function(e) NULL
    )
  }, data, groupDomains)

  # Transform all scales
  data <- lapply(data, ggfun("scales_transform_df"), scales = scales)
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  
  layout$train_position(data, scale_x(), scale_y())
  
  # Before mapping x/y position, save the domain (for discrete scales)
  # to display in tooltip.
  data <- lapply(data, function(d) {
    if (!is.null(scale_x()) && scale_x()$is_discrete()) d$x_plotlyDomain <- d$x
    if (!is.null(scale_y()) && scale_y()$is_discrete()) d$y_plotlyDomain <- d$y
    d
  })
  data <- layout$map_position(data)
  
  # build a mapping between group and key
  # if there are multiple keys within a group, the key is a list-column
  reComputeGroup <- function(x, layer = NULL) {
    # 1-to-1 link between data & visual marks -- group == key
    if ("GeomDotplot" %in% class(layer$geom)) {
      x <- split(x, x[["PANEL"]])
      x <- lapply(x, function(d) { 
        d[["group"]] <- do.call("order", d[c("x", "group")]) 
        d 
      })
      x <- dplyr::bind_rows(x)
    }
    x
  }
  
  nestedKeys <- Map(function(x, y, z) { 
    key <- y[[crosstalk_key()]]
    if (is.null(key) || inherits(z[["stat"]], "StatIdentity")) return(NULL)
    x <- reComputeGroup(x, z)
    tib <- tibble::as_tibble(x[c("PANEL", "group")])
    tib[["key"]] <- key
    nested <- tidyr::nest(tib, key, .key = key)
    # reduce the dimensions of list column elements from 2 to 1
    nested$key <- lapply(nested$key, function(x) x[[1]])
    nested
  }, data, layer_data, layers)
  
  # for some geoms (e.g. boxplots) plotly.js needs the "pre-statistics" data
  # we also now provide the option to return one of these two
  prestats_data <- data
  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))
  
  # Make sure missing (but required) aesthetics are added
  ggfun("scales_add_missing")(plot, c("x", "y"), plot$plot_env)
  
  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom_1(d))

  # compute_geom_1 can reorder the rows from `data`, making groupDomains
  # invalid. We rebuild groupDomains based on the current `data` and the
  # group map we built before.
  groupDomains <- Map(function(x, y) {
    tryCatch({
      names(y)[match(x$group, y)]
    }, error = function(e) NULL
    )
  }, data, group_maps)
  
  # there are some geoms (e.g. geom_dotplot()) where attaching the key 
  # before applying the statistic can cause problems, but there is still a 
  # 1-to-1 corresponding between graphical marks and 

  # Apply position adjustments
  data <- by_layer(function(l, d) l$compute_position(d, layout))
  
  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's
  # displayed, or does it include the range of underlying data
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)
  
  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, ggfun("scales_train_df"), scales = npscales)
    # this for loop is unique to plotly -- it saves the "domain"
    # of each non-positional scale for display in tooltips
    for (sc in npscales$scales) {
      data <- lapply(data, function(d) {
        # scale may not be relevant for every layer data
        if (any(names(d) %in% sc$aesthetics)) {
          d[paste0(sc$aesthetics, "_plotlyDomain")] <- d[sc$aesthetics]
        }
        d
      })
    }
    data <- lapply(data, ggfun("scales_map_df"), scales = npscales)
  }
  
  # Train coordinate system
  layout$train_ranges(plot$coordinates)
  
  # Fill in defaults etc.
  data <- by_layer(function(l, d) l$compute_geom_2(d))
  
  # Let layer stat have a final say before rendering
  data <- by_layer(function(l, d) l$finish_statistics(d))
  
  # Let Layout modify data before rendering
  data <- layout$finish_data(data)
  
  # ------------------------------------------------------------------------
  # end of ggplot_build()
  # ------------------------------------------------------------------------
  # if necessary, attach key
  data <- Map(function(x, y, z) { 
    if (!length(y)) return(x)
    x <- reComputeGroup(x, z)
    suppressMessages(dplyr::left_join(x, y))
  }, data, nestedKeys, layers)
  
  # initiate plotly.js layout with some plot-wide theming stuff
  theme <- ggfun("plot_theme")(plot)
  elements <- names(which(sapply(theme, inherits, "element")))
  for (i in elements) {
    theme[[i]] <- ggplot2::calc_element(i, theme)
  }
  # Translate plot wide theme elements to plotly.js layout
  pm <- unitConvert(theme$plot.margin, "pixels")
  gglayout <- list(
    margin = list(t = pm[[1]], r = pm[[2]], b = pm[[3]], l = pm[[4]]),
    plot_bgcolor = toRGB(theme$panel.background$fill),
    paper_bgcolor = toRGB(theme$plot.background$fill),
    font = text2font(theme$text)
  )
  # main plot title
  if (nchar(plot$labels$title %||% "") > 0) {
    gglayout$title <- faced(plot$labels$title, theme$plot.title$face)
    gglayout$titlefont <- text2font(theme$plot.title)
    gglayout$margin$t <- gglayout$margin$t + gglayout$titlefont$size
  }
  # ensure there's enough space for the modebar (this is based on a height of 1em)
  # https://github.com/plotly/plotly.js/blob/dd1547/src/components/modebar/index.js#L171
  gglayout$margin$t <- gglayout$margin$t + 16
  
  # important stuff like layout$panel_ranges is already flipped, but
  # plot$scales/plot$labels/data aren't. We flip x/y trace data at the very end
  # and scales in the axis loop below.
  if (inherits(plot$coordinates, "CoordFlip")) {
    plot$labels[c("x", "y")] <- plot$labels[c("y", "x")]
  }
  
  # important panel summary stats
  nPanels <- nrow(layout$panel_layout)
  nRows <- max(layout$panel_layout$ROW)
  nCols <- max(layout$panel_layout$COL)
  
  # panel -> plotly.js axis/anchor info
  # (assume a grid layout by default)
  layout$panel_layout$xaxis <- layout$panel_layout$COL
  layout$panel_layout$yaxis <- layout$panel_layout$ROW
  layout$panel_layout$xanchor <- nRows
  layout$panel_layout$yanchor <- 1
  if (inherits(plot$facet, "FacetWrap")) {
    if (plot$facet$params$free$x) {
      layout$panel_layout$xaxis <- layout$panel_layout$PANEL
      layout$panel_layout$xanchor <- layout$panel_layout$ROW
    }
    if (plot$facet$params$free$y) {
      layout$panel_layout$yaxis <- layout$panel_layout$PANEL
      layout$panel_layout$yanchor <- layout$panel_layout$COL
      layout$panel_layout$xanchor <- nPanels
    }
    if (plot$facet$params$free$x && plot$facet$params$free$y) {
      layout$panel_layout$xaxis <- layout$panel_layout$PANEL
      layout$panel_layout$yaxis <- layout$panel_layout$PANEL
      layout$panel_layout$xanchor <- layout$panel_layout$PANEL
      layout$panel_layout$yanchor <- layout$panel_layout$PANEL
    }
  }
  # format the axis/anchor to a format plotly.js respects
  layout$panel_layout$xaxis <- paste0("xaxis", sub("^1$", "", layout$panel_layout$xaxis))
  layout$panel_layout$yaxis <- paste0("yaxis", sub("^1$", "", layout$panel_layout$yaxis))
  layout$panel_layout$xanchor <- paste0("y", sub("^1$", "", layout$panel_layout$xanchor))
  layout$panel_layout$yanchor <- paste0("x", sub("^1$", "", layout$panel_layout$yanchor))
  # for some layers2traces computations, we need the range of each panel
  layout$panel_layout$x_min <- sapply(layout$panel_ranges, function(z) min(z$x.range))
  layout$panel_layout$x_max <- sapply(layout$panel_ranges, function(z) max(z$x.range))
  layout$panel_layout$y_min <- sapply(layout$panel_ranges, function(z) min(z$y.range))
  layout$panel_layout$y_max <- sapply(layout$panel_ranges, function(z) max(z$y.range))
  
  # layers -> plotly.js traces
  plot$tooltip <- tooltip
  data <- Map(function(x, y) {
    tryCatch({ x$group_plotlyDomain <- y; x }, error = function(e) x)
  }, data, groupDomains)
  
  # reattach crosstalk key-set attribute
  data <- Map(function(x, y) structure(x, set = y), data, sets)
  traces <- layers2traces(data, prestats_data, layout$panel_layout, plot)
  
  # default to just the text in hover info, mainly because of this
  # https://github.com/plotly/plotly.js/issues/320
  traces <- lapply(traces, function(tr) {
    tr$hoverinfo <- tr$hoverinfo %||%"text"
    tr
  })
  # show only one legend entry per legendgroup
  grps <- sapply(traces, "[[", "legendgroup")
  traces <- Map(function(x, y) {
    if (!is.null(x[["frame"]])) return(x)
    x$showlegend <- isTRUE(x$showlegend) && y
    x
  }, traces, !duplicated(grps))
  
  # ------------------------------------------------------------------------
  # axis/facet/margin conversion
  # ------------------------------------------------------------------------
  
  # panel margins must be computed before panel/axis loops
  # (in order to use get_domains())
  panelMarginX <- unitConvert(
    theme[["panel.spacing.x"]] %||% theme[["panel.spacing"]],
    "npc", "width"
  )
  panelMarginY <- unitConvert(
    theme[["panel.spacing.y"]] %||% theme[["panel.spacing"]],
    "npc", "height"
  )
  # space for _interior_ facet strips
  if (inherits(plot$facet, "FacetWrap")) {
    stripSize <- unitConvert(
      theme[["strip.text.x"]] %||% theme[["strip.text"]],
      "npc", "height"
    )
    panelMarginY <- panelMarginY + stripSize
    # space for ticks/text in free scales
    if (plot$facet$params$free$x) {
      axisTicksX <- unitConvert(
        theme[["axis.ticks.x"]] %||% theme[["axis.ticks"]],
        "npc", "height"
      )
      # allocate enough space for the _longest_ text label
      axisTextX <- theme[["axis.text.x"]] %||% theme[["axis.text"]]
      labz <- unlist(lapply(layout$panel_ranges, "[[", "x.labels"))
      lab <- labz[which.max(nchar(labz))]
      panelMarginY <- panelMarginY + axisTicksX +
        bbox(lab, axisTextX$angle, unitConvert(axisTextX, "npc", "height"))[["height"]]
    }
    if (plot$facet$params$free$y) {
      axisTicksY <- unitConvert(
        theme[["axis.ticks.y"]] %||% theme[["axis.ticks"]],
        "npc", "width"
      )
      # allocate enough space for the _longest_ text label
      axisTextY <- theme[["axis.text.y"]] %||% theme[["axis.text"]]
      labz <- unlist(lapply(layout$panel_ranges, "[[", "y.labels"))
      lab <- labz[which.max(nchar(labz))]
      panelMarginX <- panelMarginX + axisTicksY +
        bbox(lab, axisTextY$angle, unitConvert(axisTextY, "npc", "width"))[["width"]]
    }
  }
  margins <- c(
    rep(panelMarginX, 2),
    rep(panelMarginY, 2)
  )
  doms <- get_domains(nPanels, nRows, margins)
  
  for (i in seq_len(nPanels)) {
    lay <- layout$panel_layout[i, ]
    for (xy in c("x", "y")) {
      # find axis specific theme elements that inherit from their parent
      theme_el <- function(el) {
        theme[[paste0(el, ".", xy)]] %||% theme[[el]]
      }
      axisTicks <- theme_el("axis.ticks")
      axisText <- theme_el("axis.text")
      axisTitle <- theme_el("axis.title")
      axisLine <- theme_el("axis.line")
      panelGrid <- theme_el("panel.grid.major")
      stripText <- theme_el("strip.text")
      
      axisName <- lay[, paste0(xy, "axis")]
      anchor <- lay[, paste0(xy, "anchor")]
      rng <- layout$panel_ranges[[i]]
      # stuff like layout$panel_ranges is already flipped, but scales aren't
      sc <- if (inherits(plot$coordinates, "CoordFlip")) {
        scales$get_scales(setdiff(c("x", "y"), xy))
      } else {
        scales$get_scales(xy)
      }
      # type of unit conversion
      type <- if (xy == "x") "height" else "width"
      # get axis title
      axisTitleText <- sc$name %||% plot$labels[[xy]] %||% ""
      if (is_blank(axisTitle)) axisTitleText <- ""
      # https://plot.ly/r/reference/#layout-xaxis
      axisObj <- list(
        type = "linear",
        autorange = FALSE,
        tickmode = "array",
        range = rng[[paste0(xy, ".range")]],
        ticktext = rng[[paste0(xy, ".labels")]],
        # TODO: implement minor grid lines with another axis object
        # and _always_ hide ticks/text?
        tickvals = rng[[paste0(xy, ".major")]],
        ticks = if (is_blank(axisTicks)) "" else "outside",
        tickcolor = toRGB(axisTicks$colour),
        ticklen = unitConvert(theme$axis.ticks.length, "pixels", type),
        tickwidth = unitConvert(axisTicks, "pixels", type),
        showticklabels = !is_blank(axisText),
        tickfont = text2font(axisText, type),
        tickangle = - (axisText$angle %||% 0),
        showline = !is_blank(axisLine),
        linecolor = toRGB(axisLine$colour),
        linewidth = unitConvert(axisLine, "pixels", type),
        showgrid = !is_blank(panelGrid),
        domain = sort(as.numeric(doms[i, paste0(xy, c("start", "end"))])),
        gridcolor = toRGB(panelGrid$colour),
        gridwidth = unitConvert(panelGrid, "pixels", type),
        zeroline = FALSE,
        anchor = anchor,
        title = axisTitleText,
        titlefont = text2font(axisTitle)
      )
      # convert dates to milliseconds (86400000 = 24 * 60 * 60 * 1000)
      # this way both dates/datetimes are on same scale
      # hopefully scale_name doesn't go away -- https://github.com/hadley/ggplot2/issues/1312
      if (identical("date", sc$scale_name)) {
        axisObj$range <- axisObj$range * 86400000
        if (i == 1) {
          traces <- lapply(traces, function(z) { z[[xy]] <- z[[xy]] * 86400000; z })
        }
      }
      # tickvals are currently on 0-1 scale, but we want them on data scale
      axisObj$tickvals <- scales::rescale(
        axisObj$tickvals, to = axisObj$range, from = c(0, 1)
      )
      
      # clear out tickvals/ticktext if dynamic ticks are requested
      if (dynamicTicks) {
        axisObj[c("tickvals", "ticktext")] <- NULL
      }
      
      # attach axis object to the layout
      gglayout[[axisName]] <- axisObj
      
      # do some stuff that should be done once for the entire plot
      if (i == 1) {
        axisTickText <- axisObj$ticktext[which.max(nchar(axisObj$ticktext))]
        side <- if (xy == "x") "b" else "l"
        # account for axis ticks, ticks text, and titles in plot margins
        # (apparently ggplot2 doesn't support axis.title/axis.text margins)
        gglayout$margin[[side]] <- gglayout$margin[[side]] + axisObj$ticklen +
          bbox(axisTickText, axisObj$tickangle, axisObj$tickfont$size)[[type]] +
          bbox(axisTitleText, axisTitle$angle, unitConvert(axisTitle, "pixels", type))[[type]]
        
        if (nchar(axisTitleText) > 0) {
          axisTextSize <- unitConvert(axisText, "npc", type)
          axisTitleSize <- unitConvert(axisTitle, "npc", type)
          offset <-
            (0 -
               bbox(axisTickText, axisText$angle, axisTextSize)[[type]] -
               bbox(axisTitleText, axisTitle$angle, axisTitleSize)[[type]] / 2 -
               unitConvert(theme$axis.ticks.length, "npc", type))
        }
        
        # add space for exterior facet strips in `layout.margin`
        
        if (has_facet(plot)) {
          stripSize <- unitConvert(stripText, "pixels", type)
          if (xy == "x") {
            gglayout$margin$t <- gglayout$margin$t + stripSize
          }
          if (xy == "y" && inherits(plot$facet, "FacetGrid")) {
            gglayout$margin$r <- gglayout$margin$r + stripSize
          }
          # facets have multiple axis objects, but only one title for the plot,
          # so we empty the titles and try to draw the title as an annotation
          if (nchar(axisTitleText) > 0) {
            # npc is on a 0-1 scale of the _entire_ device,
            # but these units _should_ be wrt to the plotting region
            # multiplying the offset by 2 seems to work, but this is a terrible hack
            x <- if (xy == "x") 0.5 else offset
            y <- if (xy == "x") offset else 0.5
            gglayout$annotations <- c(
              gglayout$annotations,
              make_label(
                faced(axisTitleText, axisTitle$face), x, y, el = axisTitle,
                xanchor = if (xy == "x") "center" else "right", 
                yanchor = if (xy == "x") "top" else "center", 
                annotationType = "axis"
              )
            )
          }
        }
      }
      if (has_facet(plot)) gglayout[[axisName]]$title <- ""
    } # end of axis loop
    
    # theme(panel.border = ) -> plotly rect shape
    xdom <- gglayout[[lay[, "xaxis"]]]$domain
    ydom <- gglayout[[lay[, "yaxis"]]]$domain
    border <- make_panel_border(xdom, ydom, theme)
    gglayout$shapes <- c(gglayout$shapes, border)
    
    # facet strips -> plotly annotations
    if (has_facet(plot)) {
      col_vars <- ifelse(inherits(plot$facet, "FacetWrap"), "facets", "cols")
      col_txt <- paste(
        plot$facet$params$labeller(
          lay[names(plot$facet$params[[col_vars]])]
        ), collapse = "<br>"
      )
      if (is_blank(theme[["strip.text.x"]])) col_txt <- ""
      if (inherits(plot$facet, "FacetGrid") && lay$ROW != 1) col_txt <- ""
      if (nchar(col_txt) > 0) {
        col_lab <- make_label(
          col_txt, x = mean(xdom), y = max(ydom),
          el = theme[["strip.text.x"]] %||% theme[["strip.text"]],
          xanchor = "center", yanchor = "bottom"
        )
        gglayout$annotations <- c(gglayout$annotations, col_lab)
        strip <- make_strip_rect(xdom, ydom, theme, "top")
        gglayout$shapes <- c(gglayout$shapes, strip)
      }
      row_txt <- paste(
        plot$facet$params$labeller(
          lay[names(plot$facet$params$rows)]
        ), collapse = "<br>"
      )
      if (is_blank(theme[["strip.text.y"]])) row_txt <- ""
      if (inherits(plot$facet, "FacetGrid") && lay$COL != nCols) row_txt <- ""
      if (nchar(row_txt) > 0) {
        row_lab <- make_label(
          row_txt, x = max(xdom), y = mean(ydom),
          el = theme[["strip.text.y"]] %||% theme[["strip.text"]],
          xanchor = "left", yanchor = "middle"
        )
        gglayout$annotations <- c(gglayout$annotations, row_lab)
        strip <- make_strip_rect(xdom, ydom, theme, "right")
        gglayout$shapes <- c(gglayout$shapes, strip)
      }
    }
  } # end of panel loop
  
  # ------------------------------------------------------------------------
  # guide conversion
  #   Strategy: Obtain and translate the output of ggplot2:::guides_train().
  #   To do so, we borrow some of the body of ggplot2:::guides_build().
  # ------------------------------------------------------------------------
  # will there be a legend?
  gglayout$showlegend <- sum(unlist(lapply(traces, "[[", "showlegend"))) >= 1
  
  # legend styling
  gglayout$legend <- list(
    bgcolor = toRGB(theme$legend.background$fill),
    bordercolor = toRGB(theme$legend.background$colour),
    borderwidth = unitConvert(theme$legend.background$size, "pixels", "width"),
    font = text2font(theme$legend.text)
  )
  
  # if theme(legend.position = "none") is used, don't show a legend _or_ guide
  if (npscales$n() == 0 || identical(theme$legend.position, "none")) {
    gglayout$showlegend <- FALSE
  } else {
    # by default, guide boxes are vertically aligned
    theme$legend.box <- theme$legend.box %||% "vertical"
    
    # size of key (also used for bar in colorbar guide)
    theme$legend.key.width <- theme$legend.key.width %||% theme$legend.key.size
    theme$legend.key.height <- theme$legend.key.height %||% theme$legend.key.size
    
    # legend direction must be vertical
    theme$legend.direction <- theme$legend.direction %||% "vertical"
    if (!identical(theme$legend.direction, "vertical")) {
      warning(
        "plotly.js does not (yet) support horizontal legend items \n",
        "You can track progress here: \n",
        "https://github.com/plotly/plotly.js/issues/53 \n",
        call. = FALSE
      )
      theme$legend.direction <- "vertical"
    }
    
    # justification of legend boxes
    theme$legend.box.just <- theme$legend.box.just %||% c("center", "center")
    # scales -> data for guides
    gdefs <- ggfun("guides_train")(scales, theme, plot$guides, plot$labels)
    if (length(gdefs) > 0) {
      gdefs <- ggfun("guides_merge")(gdefs)
      gdefs <- ggfun("guides_geom")(gdefs, layers, plot$mapping)
    }
    
    # colourbar -> plotly.js colorbar
    colorbar <- compact(lapply(gdefs, gdef2trace, theme, gglayout))
    nguides <- length(colorbar) + gglayout$showlegend
    # If we have 2 or more guides, set x/y positions accordingly
    if (nguides >= 2) {
      # place legend at the bottom
      gglayout$legend$y <- 1 / nguides
      gglayout$legend$yanchor <- "top"
      # adjust colorbar position(s)
      for (i in seq_along(colorbar)) {
        colorbar[[i]]$marker$colorbar$yanchor <- "top"
        colorbar[[i]]$marker$colorbar$len <- 1 / nguides
        colorbar[[i]]$marker$colorbar$y <- 1 - (i - 1) * (1 / nguides)
      }
    }
    traces <- c(traces, colorbar)
    
    # legend title annotation - https://github.com/plotly/plotly.js/issues/276
    if (isTRUE(gglayout$showlegend)) {
      legendTitles <- compact(lapply(gdefs, function(g) if (inherits(g, "legend")) g$title else NULL))
      legendTitle <- paste(legendTitles, collapse = "<br>")
      titleAnnotation <- make_label(
        legendTitle,
        x = gglayout$legend$x %||% 1.02,
        y = gglayout$legend$y %||% 1,
        theme$legend.title,
        xanchor = "left",
        yanchor = "bottom",
        # just so the R client knows this is a title
        legendTitle = TRUE
      )
      gglayout$annotations <- c(gglayout$annotations, titleAnnotation)
      # adjust the height of the legend to accomodate for the title
      # this assumes the legend always appears below colorbars
      gglayout$legend$y <- (gglayout$legend$y %||% 1) -
        length(legendTitles) * unitConvert(theme$legend.title$size, "npc", "height")
    }
  }
  
  # geom_bar() hacks
  geoms <- sapply(layers, ggtype, "geom")
  if (any(idx <- geoms %in% c("bar", "col"))) {
    # traces were reversed in layers2traces()
    gglayout$legend$traceorder <- "reversed"
    # since `layout.barmode` is plot-specific, we can't support multiple bar
    # geoms with different positions
    positions <- sapply(layers, ggtype, "position")
    position <- unique(positions[idx])
    if (length(position) > 1) {
      warning("plotly doesn't support multiple positions\n",
              "across geom_bar() layers", call. = FALSE)
      position <- position[1]
    }
    # hacks for position_identity()
    if ("identity" %in% position) {
      gglayout$barmode <- "overlay"
    } else {
      # yes, this should work even for position_dodge()
      gglayout$barmode <- "stack"
    }
    # note: ggplot2 doesn't flip x/y scales when the coord is flipped
    # (i.e., at this point, y should be the count/density)
    is_hist <- inherits(plot$scales$get_scales("x"), "ScaleContinuous")
    # TODO: get rid of this and use explicit width for bars
    # https://github.com/plotly/plotly.js/issues/80
    if (position == "dodge" || is_hist) {
      gglayout$bargap <- 0
    }
  }
  
  # flip x/y in traces for flipped coordinates
  # (we've already done appropriate flipping for axis objects)
  if (inherits(plot$coordinates, "CoordFlip")) {
    for (i in seq_along(traces)) {
      tr <- traces[[i]]
      traces[[i]][c("x", "y")] <- tr[c("y", "x")]
      if (tr$type %in% c("bar", "box")) traces[[i]]$orientation <- "h"
      if (tr$type == "box") traces[[i]]$hoverinfo <- "x"
      names(traces[[i]])[grepl("^error_y$", names(tr))] <- "error_x"
      names(traces[[i]])[grepl("^error_x$", names(tr))] <- "error_y"
    }
  }
  
  # Error bar widths in ggplot2 are on the range of the x/y scale,
  # but plotly wants them in pixels:
  for (xy in c("x", "y")) {
    type <- if (xy == "x") "width" else "height"
    err <- if (xy == "x") "error_y" else "error_x"
    for (i in seq_along(traces)) {
      e <- traces[[i]][[err]]
      if (!is.null(e)) {
        # TODO: again, "npc" is on device scale...we really want plot scale
        w <- grid::unit(e$width %||% 0, "npc")
        traces[[i]][[err]]$width <- unitConvert(w, "pixels", type)
      }
    }
  }
  
  # try to merge marker/line traces that have the same values for these props
  props <- c("x", "y", "text", "type", "xaxis", "yaxis", "name")
  hashes <- vapply(traces, function(x) digest::digest(x[names(x) %in% props]), character(1))
  modes <- vapply(traces, function(x) x$mode %||% "", character(1))
  nhashes <- length(unique(hashes))
  if (nhashes < length(traces)) {
    mergedTraces <- vector("list", nhashes)
    for (i in unique(hashes)) {
      idx <- which(hashes %in% i)
      # for now we just merge markers and lines -- I can't imagine text being worthwhile
      if (all(modes[idx] %in% c("lines", "markers"))) {
        mergedTraces[[i]] <- Reduce(modify_list, traces[idx])
        mergedTraces[[i]]$mode <- "markers+lines"
        if (any(sapply(traces[idx], "[[", "showlegend"))) {
          mergedTraces[[i]]$showlegend <- TRUE
        }
      }
    }
    traces <- mergedTraces
  }
  
  # better layout defaults (TODO: provide a mechanism for templating defaults)
  gglayout$hovermode <- "closest"
  ax <- grep("^[x-y]axis", names(gglayout))
  for (i in ax) {
    gglayout[[i]]$hoverformat <- ".2f"
  }
  # If a trace isn't named, it shouldn't have additional hoverinfo
  traces <- lapply(compact(traces), function(x) { x$name <- x$name %||% ""; x })
  
  gglayout$width <- width
  gglayout$height <- height
  
  l <- list(
    data = setNames(traces, NULL),
    layout = compact(gglayout),
    # prevent autosize on doubleClick which clears ggplot2 margins
    config = list(doubleClick = "reset"),
    source = source
  )
  # strip any existing 'AsIs' list elements of their 'AsIs' status.
  # this is necessary since ggplot_build(qplot(1:10, fill = I("red")))
  # returns list element with their 'AsIs' class,
  # which conflicts with our JSON unboxing strategy.
  l <- rm_asis(l)
  
  # start build a plotly object with meta information about the ggplot
  # first, translate layer mappings -> plotly attrs
  mappingFormulas <- lapply(layers, function(x) {
    mappings <- c(x$mapping, if (isTRUE(x$inherit.aes)) plot$mapping)
    if (originalData) {
      lapply(mappings, lazyeval::f_new)
    } else {
      nms <- names(mappings)
      setNames(lapply(nms, function(x) lazyeval::f_new(as.symbol(x))), nms)
    }
  })
  
  return_dat <- if (originalData) layer_data else data
  
  # translate group aesthetics to data attributes
  return_dat <- Map(function(x, y) {
    if (is.null(y[["group"]])) return(x)
    dplyr::group_by_(x, y[["group"]])
  }, return_dat, mappingFormulas)
  
  # don't need to add group as an attribute anymore
  mappingFormulas <- lapply(mappingFormulas, function(x) x[!grepl("^group$", names(x))])
  
  ids <- lapply(seq_along(data), function(x) new_id())
  l$attrs <- setNames(mappingFormulas, ids)
  l$attrs <- lapply(l$attrs, function(x) structure(x, class = "plotly_eval"))
  # the build step removes the first attrs if no type exists
  l$attrs[[1]][["type"]] <- "ggplotly"
  
  l$cur_data <- ids[[layerData]]
  l$visdat <- setNames(lapply(return_dat, function(x) function(y) x), ids)

  l
}


#-----------------------------------------------------------------------------
# ggplotly 'utility' functions
#-----------------------------------------------------------------------------

# convert ggplot2 sizes and grid unit(s) to pixels or normalized point coordinates
unitConvert <- function(u, to = c("npc", "pixels"), type = c("x", "y", "height", "width")) {
  u <- verifyUnit(u)
  
  convert <- switch(
    type[1],
    x = grid::convertX,
    y = grid::convertY,
    width = grid::convertWidth,
    height = grid::convertHeight
  )
  # convert everything to npc first
  if (inherits(u, "margin")) {
    # margins consist of 4 parts: top, right, bottom, and left
    uh <- grid::convertHeight(u, "npc")
    uw <- grid::convertWidth(u, "npc")
    u <- grid::unit(c(uh[1], uw[2], uh[3], uw[4]), "npc")
  } else {
    u <- convert(u, "npc")
  }
  if (to[1] == "pixels") {
    if (inherits(u, "margin")) {
      uh <- mm2pixels(grid::convertHeight(uh, "mm"))
      uw <- mm2pixels(grid::convertWidth(uw, "mm"))
      u <- c(uh[1], uw[2], uh[3], uw[4])
    } else {
      u <- mm2pixels(convert(u, "mm"))
    }
  }
  as.numeric(u)
}

# ggplot2 size is in millimeters. plotly is in pixels. To do this correctly,
# we need to know PPI/DPI of the display. I'm not sure of a decent way to do that
# from R, but it seems 96 is a reasonable assumption.
mm2pixels <- function(u) {
  u <- verifyUnit(u)
  if (attr(u, "unit") != "mm") {
    stop("Unit must be in millimeters")
  }
  (as.numeric(u) * 96) / 25.4
}

verifyUnit <- function(u) {
  # the default unit in ggplot2 is millimeters (unless it's element_text())
  if (is.null(attr(u, "unit"))) {
    u <- if (inherits(u, "element")) {
      grid::unit(u$size %||% 0, "points")
    } else {
      grid::unit(u %||% 0, "mm")
    }
  }
  u
}

# detect a blank theme element
is_blank <- function(x) {
  inherits(x, "element_blank") && inherits(x, "element")
}

# given text, and x/y coordinates on 0-1 scale,
# convert ggplot2::element_text() to plotly annotation
make_label <- function(txt = "", x, y, el = ggplot2::element_text(), ...) {
  if (is_blank(el) || is.null(txt) || nchar(txt) == 0 || length(txt) == 0) {
    return(NULL)
  }
  angle <- el$angle %||% 0
  list(list(
    text = txt,
    x = x,
    y = y,
    showarrow = FALSE,
    # TODO: hjust/vjust?
    ax = 0,
    ay = 0,
    font = text2font(el),
    xref = "paper",
    yref = "paper",
    textangle = -angle,
    ...
  ))
}

has_facet <- function(x) {
  inherits(x$facet, c("FacetGrid", "FacetWrap"))
}

#' Estimate bounding box of a rotated string
#'
#' @param txt a character string of length 1
#' @param angle sets the angle of the tick labels with respect to the
#' horizontal (e.g., `tickangle` of -90 draws the tick labels vertically)
#' @param size vertical size of a character
#' @references
#' https://www.dropbox.com/s/nc6968prgw8ne4w/bbox.pdf?dl=0

bbox <- function(txt = "foo", angle = 0, size = 12) {
  # assuming the horizontal size of a character is roughly half of the vertical
  n <- nchar(txt)
  if (sum(n) == 0) return(list(height = 0, width = 0))
  w <- size * (nchar(txt) / 2)
  angle <- abs(angle %||% 0)
  # do the sensible thing in the majority of cases
  if (angle == 0) return(list(height = size, width = w))
  if (angle == 90) return(list(height = w, width = size))
  # first, compute the hypotenus
  hyp <- sqrt(size ^ 2 + w ^ 2)
  list(
    height = max(hyp * cos(90 - angle), size),
    width = max(hyp * sin(90 - angle), w)
  )
}

# create a plotly font object from ggplot2::element_text()
text2font <- function(x = ggplot2::element_text(), type = "height") {
  list(
    color = toRGB(x$colour),
    family = x$family,
    # TODO: what about the size of vertical text?
    size = unitConvert(grid::unit(x$size %||% 0, "points"), "pixels", type)
  )
}

# wrap text in bold/italics according to the text "face"
faced <- function(txt, face = "plain") {
  if (is.null(face)) face <- "plain"
  x <- switch(face,
              plain = txt,
              bold = bold(txt),
              italic = italic(txt),
              bold.italic = bold(italic(txt))
  )
  # if, for some reason, a face we don't support is used, return the text
  if (is.null(x)) txt else x
}
bold <- function(x) paste("<b>", x, "</b>")
italic <- function(x) paste("<i>", x, "</i>")

# if a vector that has one unique value (ignoring missings), return that value
uniq <- function(x) {
  u <- unique(x)
  if (identical(u, NA) || length(u) == 0) return(u)
  u <- u[!is.na(u)]
  if (length(u) == 1) u else x
}

# theme(strip.background) -> plotly.js rect shape
make_strip_rect <- function(xdom, ydom, theme, side = "top") {
  rekt <- rect2shape(theme[["strip.background"]])
  stripTextX <- theme[["strip.text.x"]] %||% theme[["strip.text"]]
  xTextSize <- unitConvert(stripTextX$size, "npc", "width")
  stripTextY <- theme[["strip.text.y"]] %||% theme[["strip.text"]]
  yTextSize <- unitConvert(stripTextY$size, "npc", "height")
  if ("right" %in% side) {
    # x-padding should be accounted for in `layout.margin.r`
    rekt$x0 <- xdom[2]
    rekt$x1 <- xdom[2] + xTextSize
    rekt$y0 <- ydom[1]
    rekt$y1 <- ydom[2]
  }
  if ("top" %in% side) {
    rekt$x0 <- xdom[1]
    rekt$x1 <- xdom[2]
    rekt$y0 <- ydom[2]
    rekt$y1 <- ydom[2] + yTextSize
  }
  list(rekt)
}

# theme(panel.border) -> plotly.js rect shape
make_panel_border <- function(xdom, ydom, theme) {
  rekt <- rect2shape(theme[["panel.border"]])
  rekt$x0 <- xdom[1]
  rekt$x1 <- xdom[2]
  rekt$y0 <- ydom[1]
  rekt$y1 <- ydom[2]
  list(rekt)
}

# element_rect -> plotly.js rect shape
rect2shape <- function(rekt = ggplot2::element_rect()) {
  list(
    type = "rect",
    fillcolor = toRGB(rekt$fill),
    line = list(
      color = toRGB(rekt$colour),
      width = unitConvert(rekt, "pixels", "width"),
      linetype = lty2dash(rekt$linetype)
    ),
    yref = "paper",
    xref = "paper"
  )
}

# We need access to internal ggplot2 functions in several places
# this helps us import functions in a way that R CMD check won't cry about
ggfun <- function(x) {
  tryCatch(getFromNamespace(x, "ggplot2"), error = function(e) NULL)
}

ggtype <- function(x, y = "geom") {
  sub(y, "", tolower(class(x[[y]])[1]))
}

# colourbar -> plotly.js colorbar
gdef2trace <- function(gdef, theme, gglayout) {
  if (inherits(gdef, "colorbar")) {
    # sometimes the key has missing values, which we can ignore
    gdef$key <- gdef$key[!is.na(gdef$key$.value), ]
    rng <- range(gdef$bar$value)
    gdef$bar$value <- scales::rescale(gdef$bar$value, from = rng)
    gdef$key$.value <- scales::rescale(gdef$key$.value, from = rng)
    list(
      x = gglayout$xaxis$range,
      y = gglayout$yaxis$range,
      # esentially to prevent this getting merged at a later point
      name = gdef$hash,
      type = "scatter",
      mode = "markers",
      opacity = 0,
      hoverinfo = "none",
      showlegend = FALSE,
      # do everything on a 0-1 scale
      marker = list(
        color = c(0, 1),
        colorscale = setNames(gdef$bar[c("value", "colour")], NULL),
        colorbar = list(
          bgcolor = toRGB(theme$legend.background$fill),
          bordercolor = toRGB(theme$legend.background$colour),
          borderwidth = unitConvert(
            theme$legend.background$size, "pixels", "width"
          ),
          thickness = unitConvert(
            theme$legend.key.width, "pixels", "width"
          ),
          title = gdef$title,
          titlefont = text2font(gdef$title.theme %||% theme$legend.title),
          tickmode = "array",
          ticktext = gdef$key$.label,
          tickvals = gdef$key$.value,
          tickfont = text2font(gdef$label.theme %||% theme$legend.text),
          ticklen = 2,
          len = 1/2
        )
      )
    )
  } else {
    # if plotly.js gets better support for multiple legends,
    # that conversion should go here
    NULL
  }
}
