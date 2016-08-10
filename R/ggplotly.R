#' Create plotly graphs using ggplot2 syntax
#'
#' See up-to-date documentation and examples at
#' \url{https://plot.ly/ggplot2}
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
#' @param originalData should the "original" or "scaled" data be returned?
#' @param ... arguments passed onto methods.
#' @seealso \link{signup}, \link{plot_ly}
#' @return a plotly object
#' @export
#' @author Carson Sievert
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
#' # client-side linked brushing
#' library(crosstalk)
#' d <- SharedData$new(mtcars)
#' subplot(
#'  qplot(data = d, x = mpg, y = wt),
#'  qplot(data = d, x = mpg, y = vs)
#' )
#' 
#' # client-side linked brushing in a scatterplot matrix
#' library(GGally)
#' iris$id <- seq_len(nrow(iris))
#' SharedData$new(iris, ~id) %>%
#'   ggpairs(aes(colour = Species), columns = 1:4) %>%
#'   ggplotly(tooltip = c("x", "y", "colour"))
#' }
#'
ggplotly <- function(p = ggplot2::last_plot(), width = NULL, height = NULL,
                     tooltip = "all", originalData = TRUE, ...) {
  UseMethod("ggplotly")
}

#' @export
ggplotly.ggmatrix <- function(p = ggplot2::last_plot(), width = NULL,
                              height = NULL, tooltip = "all",
                              originalData = TRUE, ...) {
  subplotList <- list()
  for (i in seq_len(p$ncol)) {
    columnList <- list()
    for (j in seq_len(p$nrow)) {
      thisPlot <- p[j, i]
      if (i == 1) {
        if (p$showYAxisPlotLabels) thisPlot <- thisPlot + ylab(p$yAxisLabels[j])
      } else {
        # y-axes are never drawn on the interior, and diagonal plots are densities,
        # so it doesn't make sense to synch zoom actions on y
        thisPlot <- thisPlot +
          theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
          )
      }
      columnList <- c(columnList, list(ggplotly(thisPlot, tooltip = tooltip)))
    }
    # conditioned on a column in a ggmatrix, the x-axis should be on the
    # same scale.
    s <- subplot(columnList, nrows = p$nrow, margin = 0.01, 
                 shareX = TRUE, titleY = TRUE) %>% hide_legend()
    subplotList <- c(subplotList, list(s))
  }
  s <- layout(subplot(subplotList, nrows = 1), width = width, height = height)
  if (nchar(p$title) > 0) {
    s <- layout(s, title = p$title)
  }
  for (i in seq_along(p$xAxisLabels)) {
    s$x$layout[[sub("^xaxis1$", "xaxis", paste0("xaxis", i))]]$title <- p$xAxisLabels[[i]]
  }
  for (i in seq_along(p$yAxisLabels)) {
    s$x$layout[[sub("^yaxis1$", "yaxis", paste0("yaxis", i))]]$title <- rev(p$yAxisLabels)[[i]]
  }
  # TODO: make this more correct
  if (length(p$yAxisLabels)) {
    s$x$layout$margin$l <- s$x$layout$margin$l + 20
  }
  
  s
}

#' @export
ggplotly.ggplot <- function(p = ggplot2::last_plot(), width = NULL,
                            height = NULL, tooltip = "all", 
                            originalData = TRUE, ...) {
  l <- gg2list(p, width = width, height = height, tooltip = tooltip, 
               originalData = originalData, ...)
  structure(as_widget(l), ggplotly = TRUE)
}

#' Convert a ggplot to a list.
#' @param p ggplot2 plot.
#' @param width Width of the plot in pixels (optional, defaults to automatic sizing).
#' @param height Height of the plot in pixels (optional, defaults to automatic sizing).
#' @param tooltip a character vector specifying which aesthetic tooltips to show in the
#' tooltip. The default, "all", means show all the aesthetic tooltips
#' (including the unofficial "text" aesthetic).
#' @param originalData should the "original" or "scaled" data be returned?
#' @param ... currently not used
#' @return a 'built' plotly object (list with names "data" and "layout").
#' @export
gg2list <- function(p, width = NULL, height = NULL, tooltip = "all", 
                    originalData = TRUE, ...) {
  # ------------------------------------------------------------------------
  # Our internal version of ggplot2::ggplot_build(). Modified from
  # https://github.com/hadley/ggplot2/blob/0cd0ba/R/plot-build.r#L18-L92
  # ------------------------------------------------------------------------
  
  # add a key aesthetic if one doesn't already exist and crosstalk key is detected
  has_crosstalk_key <- is.null(p$mapping[["key"]]) && 
    crosstalk_key() %in% names(p$data)
  if (has_crosstalk_key) {
    p$mapping <- c(p$mapping, key = as.symbol(crosstalk_key()))
  }
  
  p <- ggfun("plot_clone")(p)
  if (length(p$layers) == 0) {
    p <- p + geom_blank()
  }
  layers <- p$layers
  layer_data <- lapply(layers, function(y) y$layer_data(p$data))
  
  # save crosstalk sets before this attribute gets squashed
  sets <- lapply(layer_data, function(y) attr(y, "set"))
  
  scales <- p$scales
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }
  panel <- ggfun("new_panel")()
  panel <- ggfun("train_layout")(panel, p$facet, layer_data, p$data)
  data <- ggfun("map_layout")(panel, p$facet, layer_data)
  # save the domain of the group for display in tooltips
  groupDomains <- Map(function(x, y) {
    tryCatch(
      eval(y$mapping[["group"]] %||% p$mapping[["group"]], x), 
      error = function(e) NULL
    )
  }, data, p$layers)
  data <- by_layer(function(l, d) l$compute_aesthetics(d, p))
  data <- lapply(data, ggfun("scales_transform_df"), scales = scales)
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  panel <- ggfun("train_position")(panel, data, scale_x(), scale_y())
  # Before mapping x/y position, save the domain (for discrete scales)
  # to display in tooltip.
  data <- lapply(data, function(d) {
    if (!is.null(scale_x()) && scale_x()$is_discrete()) d$x_plotlyDomain <- d$x
    if (!is.null(scale_y()) && scale_y()$is_discrete()) d$y_plotlyDomain <- d$y
    d
  })
  data <- ggfun("map_position")(panel, data, scale_x(), scale_y())
  # for some geoms (e.g. boxplots) plotly.js needs the "pre-statistics" data
  prestats_data <- data
  data <- by_layer(function(l, d) l$compute_statistic(d, panel))
  data <- by_layer(function(l, d) l$map_statistic(d, p))
  ggfun("scales_add_missing")(p, c("x", "y"), p$plot_env)
  data <- by_layer(function(l, d) l$compute_geom_1(d))
  data <- by_layer(function(l, d) l$compute_position(d, p))
  ggfun("reset_scales")(panel)
  panel <- ggfun("train_position")(panel, data, scale_x(), scale_y())
  data <- ggfun("map_position")(panel, data, scale_x(), scale_y())
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
  panel <- ggfun("train_ranges")(panel, p$coordinates)
  data <- by_layer(function(l, d) l$compute_geom_2(d))
  
  # ------------------------------------------------------------------------
  # end of ggplot_build()
  # ------------------------------------------------------------------------
  # initiate plotly.js layout with some plot-wide theming stuff
  theme <- ggfun("plot_theme")(p)
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
  if (nchar(p$labels$title %||% "") > 0) {
    gglayout$title <- faced(p$labels$title, theme$plot.title$face)
    gglayout$titlefont <- text2font(theme$plot.title)
    gglayout$margin$t <- gglayout$margin$t + gglayout$titlefont$size
  }
  # ensure there's enough space for the modebar (this is based on a height of 1em)
  # https://github.com/plotly/plotly.js/blob/dd1547/src/components/modebar/index.js#L171
  gglayout$margin$t <- gglayout$margin$t + 16
  
  # important stuff like panel$ranges is already flipped, but
  # p$scales/p$labels/data aren't. We flip x/y trace data at the very end
  # and scales in the axis loop below.
  if (inherits(p$coordinates, "CoordFlip")) {
    p$labels[c("x", "y")] <- p$labels[c("y", "x")]
  }
  
  # important panel summary stats
  nPanels <- nrow(panel$layout)
  nRows <- max(panel$layout$ROW)
  nCols <- max(panel$layout$COL)
  
  # panel -> plotly.js axis/anchor info
  # (assume a grid layout by default)
  panel$layout$xaxis <- panel$layout$COL
  panel$layout$yaxis <- panel$layout$ROW
  panel$layout$xanchor <- nRows
  panel$layout$yanchor <- 1
  if (inherits(p$facet, "wrap")) {
    if (p$facet$free$x) {
      panel$layout$xaxis <- panel$layout$PANEL
      panel$layout$xanchor <- panel$layout$ROW
    }
    if (p$facet$free$y) {
      panel$layout$yaxis <- panel$layout$PANEL
      panel$layout$yanchor <- panel$layout$COL
      panel$layout$xanchor <- nPanels
    }
    if (p$facet$free$x && p$facet$free$y) {
      panel$layout$xaxis <- panel$layout$PANEL
      panel$layout$yaxis <- panel$layout$PANEL
      panel$layout$xanchor <- panel$layout$PANEL
      panel$layout$yanchor <- panel$layout$PANEL
    }
  }
  # format the axis/anchor to a format plotly.js respects
  panel$layout$xaxis <- paste0("xaxis", sub("^1$", "", panel$layout$xaxis))
  panel$layout$yaxis <- paste0("yaxis", sub("^1$", "", panel$layout$yaxis))
  panel$layout$xanchor <- paste0("y", sub("^1$", "", panel$layout$xanchor))
  panel$layout$yanchor <- paste0("x", sub("^1$", "", panel$layout$yanchor))
  # for some layers2traces computations, we need the range of each panel
  panel$layout$x_min <- sapply(panel$ranges, function(z) min(z$x.range))
  panel$layout$x_max <- sapply(panel$ranges, function(z) max(z$x.range))
  panel$layout$y_min <- sapply(panel$ranges, function(z) min(z$y.range))
  panel$layout$y_max <- sapply(panel$ranges, function(z) max(z$y.range))
  
  # layers -> plotly.js traces
  p$tooltip <- tooltip
  data <- Map(function(x, y) {
    tryCatch({ x$group_plotlyDomain <- y; x }, error = function(e) x)
  }, data, groupDomains)
  data <- Map(function(x, y) structure(x, set = y), data, sets)
  traces <- layers2traces(data, prestats_data, panel$layout, p)
  
  # default to just the text in hover info, mainly because of this
  # https://github.com/plotly/plotly.js/issues/320
  traces <- lapply(traces, function(tr) {
    tr$hoverinfo <- tr$hoverinfo %||%"text"
    tr
  })
  # show only one legend entry per legendgroup
  grps <- sapply(traces, "[[", "legendgroup")
  traces <- Map(function(x, y) {
    x$showlegend <- isTRUE(x$showlegend) && y
    x
  }, traces, !duplicated(grps))
  
  # ------------------------------------------------------------------------
  # axis/facet/margin conversion
  # ------------------------------------------------------------------------
  
  # panel margins must be computed before panel/axis loops
  # (in order to use get_domains())
  panelMarginX <- unitConvert(
    theme[["panel.margin.x"]] %||% theme[["panel.margin"]],
    "npc", "width"
  )
  panelMarginY <- unitConvert(
    theme[["panel.margin.y"]] %||% theme[["panel.margin"]],
    "npc", "height"
  )
  # space for _interior_ facet strips
  if (inherits(p$facet, "wrap")) {
    stripSize <- unitConvert(
      theme[["strip.text.x"]] %||% theme[["strip.text"]],
      "npc", "height"
    )
    panelMarginY <- panelMarginY + stripSize
    # space for ticks/text in free scales
    if (p$facet$free$x) {
      axisTicksX <- unitConvert(
        theme[["axis.ticks.x"]] %||% theme[["axis.ticks"]],
        "npc", "height"
      )
      # allocate enough space for the _longest_ text label
      axisTextX <- theme[["axis.text.x"]] %||% theme[["axis.text"]]
      labz <- unlist(lapply(panel$ranges, "[[", "x.labels"))
      lab <- labz[which.max(nchar(labz))]
      panelMarginY <- panelMarginY + axisTicksX +
        bbox(lab, axisTextX$angle, unitConvert(axisTextX, "npc", "height"))[["height"]]
    }
    if (p$facet$free$y) {
      axisTicksY <- unitConvert(
        theme[["axis.ticks.y"]] %||% theme[["axis.ticks"]],
        "npc", "width"
      )
      # allocate enough space for the _longest_ text label
      axisTextY <- theme[["axis.text.y"]] %||% theme[["axis.text"]]
      labz <- unlist(lapply(panel$ranges, "[[", "y.labels"))
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
    lay <- panel$layout[i, ]
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
      rng <- panel$ranges[[i]]
      # stuff like panel$ranges is already flipped, but scales aren't
      sc <- if (inherits(p$coordinates, "CoordFlip")) {
        scales$get_scales(setdiff(c("x", "y"), xy))
      } else {
        scales$get_scales(xy)
      }
      # type of unit conversion
      type <- if (xy == "x") "height" else "width"
      # get axis title
      axisTitleText <- sc$name %||% p$labels[[xy]] %||% ""
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
        if (has_facet(p)) {
          stripSize <- unitConvert(stripText, "pixels", type)
          if (xy == "x") {
            gglayout$margin$t <- gglayout$margin$t + stripSize
          }
          if (xy == "y" && inherits(p$facet, "grid")) {
            gglayout$margin$r <- gglayout$margin$r + stripSize
          }
          # facets have multiple axis objects, but only one title for the plot,
          # so we empty the titles and try to draw the title as an annotation
          if (nchar(axisTitleText) > 0) {
            # npc is on a 0-1 scale of the _entire_ device,
            # but these units _should_ be wrt to the plotting region
            # multiplying the offset by 2 seems to work, but this is a terrible hack
            offset <- 1.75 * offset
            x <- if (xy == "x") 0.5 else offset
            y <- if (xy == "x") offset else 0.5
            gglayout$annotations <- c(
              gglayout$annotations,
              make_label(
                faced(axisTitleText, axisTitle$face), x, y, el = axisTitle,
                xanchor = "center", yanchor = "middle", annotationType = "axis"
              )
            )
          }
        }
      }
      if (has_facet(p)) gglayout[[axisName]]$title <- ""
    } # end of axis loop
    
    # theme(panel.border = ) -> plotly rect shape
    xdom <- gglayout[[lay[, "xaxis"]]]$domain
    ydom <- gglayout[[lay[, "yaxis"]]]$domain
    border <- make_panel_border(xdom, ydom, theme)
    gglayout$shapes <- c(gglayout$shapes, border)
    
    # facet strips -> plotly annotations
    if (has_facet(p)) {
      col_vars <- ifelse(inherits(p$facet, "wrap"), "facets", "cols")
      col_txt <- paste(
        p$facet$labeller(lay[names(p$facet[[col_vars]])]), collapse = "<br>"
      )
      if (is_blank(theme[["strip.text.x"]])) col_txt <- ""
      if (inherits(p$facet, "grid") && lay$ROW != 1) col_txt <- ""
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
        p$facet$labeller(lay[names(p$facet$rows)]), collapse = "<br>"
      )
      if (is_blank(theme[["strip.text.y"]])) row_txt <- ""
      if (inherits(p$facet, "grid") && lay$COL != nCols) row_txt <- ""
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
    gdefs <- ggfun("guides_train")(scales, theme, p$guides, p$labels)
    if (length(gdefs) > 0) {
      gdefs <- ggfun("guides_merge")(gdefs)
      gdefs <- ggfun("guides_geom")(gdefs, layers, p$mapping)
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
        yanchor = "top",
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
  if (any(idx <- geoms %in% "bar")) {
    # since `layout.barmode` is plot-specific, we can't support multiple bar
    # geoms with different positions
    positions <- sapply(layers, ggtype, "position")
    position <- unique(positions[geoms %in% "bar"])
    if (length(position) > 1) {
      warning("plotly doesn't support multiple positions\n",
              "across geom_bar() layers", call. = FALSE)
      position <- position[1]
    }
    # hacks for position_identity()
    if (position == "identity") {
      gglayout$barmode <- "overlay"
      gglayout$legend$traceorder <- "reversed"
    } else {
      gglayout$barmode <- "stack"
    }
    # note: ggplot2 doesn't flip x/y scales when the coord is flipped
    # (i.e., at this point, y should be the count/density)
    is_hist <- inherits(p$scales$get_scales("x"), "ScaleContinuous")
    # TODO: get rid of this and use explicit width for bars
    # https://github.com/plotly/plotly.js/issues/80
    if (position == "dodge" || is_hist) {
      gglayout$bargap <- 0
    }
  }
  
  # flip x/y in traces for flipped coordinates
  # (we've already done appropriate flipping for axis objects)
  if (inherits(p$coordinates, "CoordFlip")) {
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
    layout = compact(gglayout)
  )
  # strip any existing 'AsIs' list elements of their 'AsIs' status.
  # this is necessary since ggplot_build(qplot(1:10, fill = I("red")))
  # returns list element with their 'AsIs' class,
  # which conflicts with our JSON unboxing strategy.
  l <- rm_asis(l)
  l$cur_data <- new_id()
  # translate "plot-wide" aesthetic mappings to formulas so plotly_build() 
  # understands them
  mappingFormulas <- if (originalData) {
    lapply(p$mapping, lazyeval::f_new)
  } else {
    nms <- names(p$mapping)
    setNames(lapply(nms, function(x) lazyeval::f_new(as.symbol(x))), nms)
  }
  # TODO: if exposing "scaled" data, how do we ensure it is "global"?
  dat <- if (originalData) p$data else data[[1]]
  if (!is.null(mappingFormulas[["group"]])) {
    dat <- dplyr::group_by_(dat, mappingFormulas[["group"]])
  }
  # don't need to add group as an attribute anymore
  mappingFormulas <- mappingFormulas[!grepl("^group$", names(mappingFormulas))]
  l$attrs <- setNames(list(mappingFormulas), l$cur_data)
  l$visdat <- setNames(list(function() dat), l$cur_data)
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
  inherits(x$facet, c("grid", "wrap"))
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
