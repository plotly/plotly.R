# copy/pasted from View(plotly::gg2list) with packageVersion("plotly") == "4.5.6"
gg2list_legacy <- function(p, width = NULL, height = NULL, tooltip = "all", layerData = 1, 
                            originalData = TRUE, source = "A", ...) 
{
  deviceWidth <- width %||% unitConvert(grid::unit(1, "npc"), 
                                        "pixels", "width")
  deviceHeight <- height %||% unitConvert(grid::unit(1, "npc"), 
                                          "pixels", "height")
  dev_fun <- if (capabilities("png")) {
    grDevices::png
  }
  else if (capabilities("jpeg")) {
    grDevices::jpeg
  }
  else {
    warning("Couldn't find a bitmap device (e.g. png or jpeg).", 
            "To ensure sizes are converted correctly please", 
            "compile R to use a bitmap device", call. = FALSE)
    grDevices::dev.new
  }
  tmpPlotFile <- tempfile(fileext = ".png")
  dev_fun(tmpPlotFile, width = deviceWidth, height = deviceHeight)
  plot <- ggfun("plot_clone")(p)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$layer_data(plot$data))
  sets <- lapply(layer_data, function(y) attr(y, "set"))
  scales <- plot$scales
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }
  layout <- ggfun("create_layout")(plot$facet)
  data <- layout$setup(layer_data, plot$data, plot$plot_env, 
                       plot$coordinates)
  data <- layout$map(data)
  groupDomains <- Map(function(x, y) {
    tryCatch(eval(y$mapping[["group"]] %||% plot$mapping[["group"]], 
                  x), error = function(e) NULL)
  }, data, layers)
  data <- by_layer(function(l, d) l$compute_aesthetics(d, 
                                                       plot))
  group_maps <- Map(function(x, y) {
    tryCatch({
      x_group <- x[["group"]]
      names(x_group) <- y
      x_group <- x_group[!duplicated(x_group)]
      x_group
    }, error = function(e) NULL)
  }, data, groupDomains)
  data <- lapply(data, ggfun("scales_transform_df"), scales = scales)
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  layout$train_position(data, scale_x(), scale_y())
  data <- lapply(data, function(d) {
    if (!is.null(scale_x()) && scale_x()$is_discrete()) 
      d$x_plotlyDomain <- d$x
    if (!is.null(scale_y()) && scale_y()$is_discrete()) 
      d$y_plotlyDomain <- d$y
    d
  })
  data <- layout$map_position(data)
  prestats_data <- data
  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))
  ggfun("scales_add_missing")(plot, c("x", "y"), plot$plot_env)
  data <- by_layer(function(l, d) l$compute_geom_1(d))
  groupDomains <- Map(function(x, y) {
    tryCatch({
      names(y)[match(x$group, y)]
    }, error = function(e) NULL)
  }, data, group_maps)
  data <- by_layer(function(l, d) l$compute_position(d, layout))
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, ggfun("scales_train_df"), scales = npscales)
    for (sc in npscales$scales) {
      data <- lapply(data, function(d) {
        if (any(names(d) %in% sc$aesthetics)) {
          d[paste0(sc$aesthetics, "_plotlyDomain")] <- d[sc$aesthetics]
        }
        d
      })
    }
    data <- lapply(data, ggfun("scales_map_df"), scales = npscales)
  }
  layout$train_ranges(plot$coordinates)
  data <- by_layer(function(l, d) l$compute_geom_2(d))
  data <- by_layer(function(l, d) l$finish_statistics(d))
  data <- layout$finish_data(data)
  theme <- ggfun("plot_theme")(plot)
  elements <- names(which(sapply(theme, inherits, "element")))
  for (i in elements) {
    theme[[i]] <- ggplot2::calc_element(i, theme)
  }
  pm <- unitConvert(theme$plot.margin, "pixels")
  gglayout <- list(margin = list(t = pm[[1]], r = pm[[2]], 
                                 b = pm[[3]], l = pm[[4]]), plot_bgcolor = toRGB(theme$panel.background$fill), 
                   paper_bgcolor = toRGB(theme$plot.background$fill), font = text2font(theme$text))
  if (nchar(plot$labels$title %||% "") > 0) {
    gglayout$title <- faced(plot$labels$title, theme$plot.title$face)
    gglayout$titlefont <- text2font(theme$plot.title)
    gglayout$margin$t <- gglayout$margin$t + gglayout$titlefont$size
  }
  gglayout$margin$t <- gglayout$margin$t + 16
  if (inherits(plot$coordinates, "CoordFlip")) {
    plot$labels[c("x", "y")] <- plot$labels[c("y", "x")]
  }
  nPanels <- nrow(layout$panel_layout)
  nRows <- max(layout$panel_layout$ROW)
  nCols <- max(layout$panel_layout$COL)
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
  layout$panel_layout$xaxis <- paste0("xaxis", sub("^1$", 
                                                   "", layout$panel_layout$xaxis))
  layout$panel_layout$yaxis <- paste0("yaxis", sub("^1$", 
                                                   "", layout$panel_layout$yaxis))
  layout$panel_layout$xanchor <- paste0("y", sub("^1$", "", 
                                                 layout$panel_layout$xanchor))
  layout$panel_layout$yanchor <- paste0("x", sub("^1$", "", 
                                                 layout$panel_layout$yanchor))
  layout$panel_layout$x_min <- sapply(layout$panel_ranges, 
                                      function(z) min(z$x.range))
  layout$panel_layout$x_max <- sapply(layout$panel_ranges, 
                                      function(z) max(z$x.range))
  layout$panel_layout$y_min <- sapply(layout$panel_ranges, 
                                      function(z) min(z$y.range))
  layout$panel_layout$y_max <- sapply(layout$panel_ranges, 
                                      function(z) max(z$y.range))
  plot$tooltip <- tooltip
  data <- Map(function(x, y) {
    tryCatch({
      x$group_plotlyDomain <- y
      x
    }, error = function(e) x)
  }, data, groupDomains)
  traces <- layers2traces(data, prestats_data, layout$panel_layout, 
                          plot)
  gglayout <- layers2layout(gglayout, layers, layout$panel_layout)
  traces <- lapply(traces, function(tr) {
    tr$hoverinfo <- tr$hoverinfo %||% "text"
    tr
  })
  grps <- sapply(traces, "[[", "legendgroup")
  traces <- Map(function(x, y) {
    x$showlegend <- isTRUE(x$showlegend) && y
    x
  }, traces, !duplicated(grps))
  panelMarginX <- unitConvert(theme[["panel.spacing.x"]] %||% 
                                theme[["panel.spacing"]], "npc", "width")
  panelMarginY <- unitConvert(theme[["panel.spacing.y"]] %||% 
                                theme[["panel.spacing"]], "npc", "height")
  if (inherits(plot$facet, "FacetWrap")) {
    stripSize <- unitConvert(theme[["strip.text.x"]] %||% 
                               theme[["strip.text"]], "npc", "height")
    panelMarginY <- panelMarginY + stripSize
    if (plot$facet$params$free$x) {
      axisTicksX <- unitConvert(theme[["axis.ticks.x"]] %||% 
                                  theme[["axis.ticks"]], "npc", "height")
      axisTextX <- theme[["axis.text.x"]] %||% theme[["axis.text"]]
      labz <- unlist(lapply(layout$panel_ranges, "[[", 
                            "x.labels"))
      lab <- labz[which.max(nchar(labz))]
      panelMarginY <- panelMarginY + axisTicksX + bbox(lab, 
                                                       axisTextX$angle, unitConvert(axisTextX, "npc", 
                                                                                    "height"))[["height"]]
    }
    if (plot$facet$params$free$y) {
      axisTicksY <- unitConvert(theme[["axis.ticks.y"]] %||% 
                                  theme[["axis.ticks"]], "npc", "width")
      axisTextY <- theme[["axis.text.y"]] %||% theme[["axis.text"]]
      labz <- unlist(lapply(layout$panel_ranges, "[[", 
                            "y.labels"))
      lab <- labz[which.max(nchar(labz))]
      panelMarginX <- panelMarginX + axisTicksY + bbox(lab, 
                                                       axisTextY$angle, unitConvert(axisTextY, "npc", 
                                                                                    "width"))[["width"]]
    }
  }
  margins <- c(rep(panelMarginX, 2), rep(panelMarginY, 2))
  doms <- get_domains(nPanels, nRows, margins)
  for (i in seq_len(nPanels)) {
    lay <- layout$panel_layout[i, ]
    for (xy in c("x", "y")) {
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
      sc <- if (inherits(plot$coordinates, "CoordFlip")) {
        scales$get_scales(setdiff(c("x", "y"), xy))
      }
      else {
        scales$get_scales(xy)
      }
      type <- if (xy == "x") 
        "height"
      else "width"
      axisTitleText <- sc$name %||% plot$labels[[xy]] %||% 
        ""
      if (is_blank(axisTitle)) 
        axisTitleText <- ""
      axisObj <- list(type = "linear", autorange = FALSE, 
                      tickmode = "array", range = rng[[paste0(xy, 
                                                              ".range")]], ticktext = rng[[paste0(xy, ".labels")]], 
                      tickvals = rng[[paste0(xy, ".major")]], ticks = if (is_blank(axisTicks)) "" else "outside", 
                      tickcolor = toRGB(axisTicks$colour), ticklen = unitConvert(theme$axis.ticks.length, 
                                                                                 "pixels", type), tickwidth = unitConvert(axisTicks, 
                                                                                                                          "pixels", type), showticklabels = !is_blank(axisText), 
                      tickfont = text2font(axisText, type), tickangle = -(axisText$angle %||% 
                                                                            0), showline = !is_blank(axisLine), linecolor = toRGB(axisLine$colour), 
                      linewidth = unitConvert(axisLine, "pixels", 
                                              type), showgrid = !is_blank(panelGrid), domain = sort(as.numeric(doms[i, 
                                                                                                                    paste0(xy, c("start", "end"))])), gridcolor = toRGB(panelGrid$colour), 
                      gridwidth = unitConvert(panelGrid, "pixels", 
                                              type), zeroline = FALSE, anchor = anchor, 
                      title = axisTitleText, titlefont = text2font(axisTitle))
      if (identical("date", sc$scale_name)) {
        axisObj$range <- axisObj$range * 86400000
        if (i == 1) {
          traces <- lapply(traces, function(z) {
            z[[xy]] <- z[[xy]] * 86400000
            z
          })
        }
      }
      axisObj$tickvals <- scales::rescale(axisObj$tickvals, 
                                          to = axisObj$range, from = c(0, 1))
      gglayout[[axisName]] <- axisObj
      if (i == 1) {
        axisTickText <- axisObj$ticktext[which.max(nchar(axisObj$ticktext))]
        side <- if (xy == "x") 
          "b"
        else "l"
        gglayout$margin[[side]] <- gglayout$margin[[side]] + 
          axisObj$ticklen + bbox(axisTickText, axisObj$tickangle, 
                                 axisObj$tickfont$size)[[type]] + bbox(axisTitleText, 
                                                                       axisTitle$angle, unitConvert(axisTitle, "pixels", 
                                                                                                    type))[[type]]
        if (nchar(axisTitleText) > 0) {
          axisTextSize <- unitConvert(axisText, "npc", 
                                      type)
          axisTitleSize <- unitConvert(axisTitle, "npc", 
                                       type)
          offset <- (0 - bbox(axisTickText, axisText$angle, 
                              axisTextSize)[[type]] - bbox(axisTitleText, 
                                                           axisTitle$angle, axisTitleSize)[[type]]/2 - 
                       unitConvert(theme$axis.ticks.length, "npc", 
                                   type))
        }
        if (has_facet(plot)) {
          stripSize <- unitConvert(stripText, "pixels", 
                                   type)
          if (xy == "x") {
            gglayout$margin$t <- gglayout$margin$t + 
              stripSize
          }
          if (xy == "y" && inherits(plot$facet, "FacetGrid")) {
            gglayout$margin$r <- gglayout$margin$r + 
              stripSize
          }
          if (nchar(axisTitleText) > 0) {
            x <- if (xy == "x") 
              0.5
            else offset
            y <- if (xy == "x") 
              offset
            else 0.5
            gglayout$annotations <- c(gglayout$annotations, 
                                      make_label(faced(axisTitleText, axisTitle$face), 
                                                 x, y, el = axisTitle, xanchor = if (xy == 
                                                                                     "x") "center" else "right", yanchor = if (xy == 
                                                                                                                               "x") "top" else "center", annotationType = "axis"))
          }
        }
      }
      if (has_facet(plot)) 
        gglayout[[axisName]]$title <- ""
    }
    xdom <- gglayout[[lay[, "xaxis"]]]$domain
    ydom <- gglayout[[lay[, "yaxis"]]]$domain
    border <- make_panel_border(xdom, ydom, theme)
    gglayout$shapes <- c(gglayout$shapes, border)
    if (has_facet(plot)) {
      col_vars <- ifelse(inherits(plot$facet, "FacetWrap"), 
                         "facets", "cols")
      col_txt <- paste(plot$facet$params$labeller(lay[names(plot$facet$params[[col_vars]])]), 
                       collapse = "<br>")
      if (is_blank(theme[["strip.text.x"]])) 
        col_txt <- ""
      if (inherits(plot$facet, "FacetGrid") && lay$ROW != 
          1) 
        col_txt <- ""
      if (nchar(col_txt) > 0) {
        col_lab <- make_label(col_txt, x = mean(xdom), 
                              y = max(ydom), el = theme[["strip.text.x"]] %||% 
                                theme[["strip.text"]], xanchor = "center", 
                              yanchor = "bottom")
        gglayout$annotations <- c(gglayout$annotations, 
                                  col_lab)
        strip <- make_strip_rect(xdom, ydom, theme, 
                                 "top")
        gglayout$shapes <- c(gglayout$shapes, strip)
      }
      row_txt <- paste(plot$facet$params$labeller(lay[names(plot$facet$params$rows)]), 
                       collapse = "<br>")
      if (is_blank(theme[["strip.text.y"]])) 
        row_txt <- ""
      if (inherits(plot$facet, "FacetGrid") && lay$COL != 
          nCols) 
        row_txt <- ""
      if (nchar(row_txt) > 0) {
        row_lab <- make_label(row_txt, x = max(xdom), 
                              y = mean(ydom), el = theme[["strip.text.y"]] %||% 
                                theme[["strip.text"]], xanchor = "left", 
                              yanchor = "middle")
        gglayout$annotations <- c(gglayout$annotations, 
                                  row_lab)
        strip <- make_strip_rect(xdom, ydom, theme, 
                                 "right")
        gglayout$shapes <- c(gglayout$shapes, strip)
      }
    }
  }
  gglayout$showlegend <- sum(unlist(lapply(traces, "[[", "showlegend"))) >= 
    1
  gglayout$legend <- list(bgcolor = toRGB(theme$legend.background$fill), 
                          bordercolor = toRGB(theme$legend.background$colour), 
                          borderwidth = unitConvert(theme$legend.background$size, 
                                                    "pixels", "width"), font = text2font(theme$legend.text))
  if (npscales$n() == 0 || identical(theme$legend.position, 
                                     "none")) {
    gglayout$showlegend <- FALSE
  }
  else {
    theme$legend.box <- theme$legend.box %||% "vertical"
    theme$legend.key.width <- theme$legend.key.width %||% 
      theme$legend.key.size
    theme$legend.key.height <- theme$legend.key.height %||% 
      theme$legend.key.size
    theme$legend.direction <- theme$legend.direction %||% 
      "vertical"
    if (!identical(theme$legend.direction, "vertical")) {
      warning("plotly.js does not (yet) support horizontal legend items \n", 
              "You can track progress here: \n", "https://github.com/plotly/plotly.js/issues/53 \n", 
              call. = FALSE)
      theme$legend.direction <- "vertical"
    }
    theme$legend.box.just <- theme$legend.box.just %||% 
      c("center", "center")
    gdefs <- ggfun("guides_train")(scales, theme, plot$guides, 
                                   plot$labels)
    if (length(gdefs) > 0) {
      gdefs <- ggfun("guides_merge")(gdefs)
      gdefs <- ggfun("guides_geom")(gdefs, layers, plot$mapping)
    }
    colorbar <- compact(lapply(gdefs, gdef2trace, theme, 
                               gglayout))
    nguides <- length(colorbar) + gglayout$showlegend
    if (nguides >= 2) {
      gglayout$legend$y <- 1/nguides
      gglayout$legend$yanchor <- "top"
      for (i in seq_along(colorbar)) {
        colorbar[[i]]$marker$colorbar$yanchor <- "top"
        colorbar[[i]]$marker$colorbar$len <- 1/nguides
        colorbar[[i]]$marker$colorbar$y <- 1 - (i - 
                                                  1) * (1/nguides)
      }
    }
    traces <- c(traces, colorbar)
    if (isTRUE(gglayout$showlegend)) {
      legendTitles <- compact(lapply(gdefs, function(g) if (inherits(g, 
                                                                     "legend")) 
        g$title
        else NULL))
      legendTitle <- paste(legendTitles, collapse = "<br>")
      titleAnnotation <- make_label(legendTitle, x = gglayout$legend$x %||% 
                                      1.02, y = gglayout$legend$y %||% 1, theme$legend.title, 
                                    xanchor = "left", yanchor = "bottom", legendTitle = TRUE)
      gglayout$annotations <- c(gglayout$annotations, 
                                titleAnnotation)
      gglayout$legend$y <- (gglayout$legend$y %||% 1) - 
        length(legendTitles) * unitConvert(theme$legend.title$size, 
                                           "npc", "height")
    }
  }
  geoms <- sapply(layers, ggtype, "geom")
  if (any(idx <- geoms %in% "bar")) {
    positions <- sapply(layers, ggtype, "position")
    position <- unique(positions[geoms %in% "bar"])
    if (length(position) > 1) {
      warning("plotly doesn't support multiple positions\n", 
              "across geom_bar() layers", call. = FALSE)
      position <- position[1]
    }
    if (position == "identity") {
      gglayout$barmode <- "overlay"
      gglayout$legend$traceorder <- "reversed"
    }
    else {
      gglayout$barmode <- "stack"
    }
    is_hist <- inherits(plot$scales$get_scales("x"), "ScaleContinuous")
    if (position == "dodge" || is_hist) {
      gglayout$bargap <- 0
    }
  }
  if (inherits(plot$coordinates, "CoordFlip")) {
    for (i in seq_along(traces)) {
      tr <- traces[[i]]
      traces[[i]][c("x", "y")] <- tr[c("y", "x")]
      if (tr$type %in% c("bar", "box")) 
        traces[[i]]$orientation <- "h"
      if (tr$type == "box") 
        traces[[i]]$hoverinfo <- "x"
      names(traces[[i]])[grepl("^error_y$", names(tr))] <- "error_x"
      names(traces[[i]])[grepl("^error_x$", names(tr))] <- "error_y"
    }
  }
  for (xy in c("x", "y")) {
    type <- if (xy == "x") 
      "width"
    else "height"
    err <- if (xy == "x") 
      "error_y"
    else "error_x"
    for (i in seq_along(traces)) {
      e <- traces[[i]][[err]]
      if (!is.null(e)) {
        w <- grid::unit(e$width %||% 0, "npc")
        traces[[i]][[err]]$width <- unitConvert(w, "pixels", 
                                                type)
      }
    }
  }
  props <- c("x", "y", "text", "type", "xaxis", "yaxis", "name")
  hashes <- vapply(traces, function(x) digest::digest(x[names(x) %in% 
                                                          props]), character(1))
  modes <- vapply(traces, function(x) x$mode %||% "", character(1))
  nhashes <- length(unique(hashes))
  if (nhashes < length(traces)) {
    mergedTraces <- vector("list", nhashes)
    for (i in unique(hashes)) {
      idx <- which(hashes %in% i)
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
  gglayout$hovermode <- "closest"
  ax <- grep("^[x-y]axis", names(gglayout))
  for (i in ax) {
    gglayout[[i]]$hoverformat <- ".2f"
  }
  traces <- lapply(compact(traces), function(x) {
    x$name <- x$name %||% ""
    x
  })
  gglayout$width <- width
  gglayout$height <- height
  grDevices::dev.off()
  unlink(tmpPlotFile)
  l <- list(data = setNames(traces, NULL), layout = compact(gglayout), 
            source = source)
  l <- rm_asis(l)
  mappingFormulas <- lapply(layers, function(x) {
    mappings <- c(x$mapping, if (isTRUE(x$inherit.aes)) plot$mapping)
    if (originalData) {
      lapply(mappings, lazyeval::f_new)
    }
    else {
      nms <- names(mappings)
      setNames(lapply(nms, function(x) lazyeval::f_new(as.symbol(x))), 
               nms)
    }
  })
  return_dat <- if (originalData) 
    layer_data
  else data
  return_dat <- Map(function(x, y) {
    if (is.null(y[["group"]])) 
      return(x)
    dplyr::group_by_(x, y[["group"]])
  }, return_dat, mappingFormulas)
  mappingFormulas <- lapply(mappingFormulas, function(x) x[!grepl("^group$", 
                                                                  names(x))])
  ids <- lapply(seq_along(data), function(x) new_id())
  l$attrs <- setNames(mappingFormulas, ids)
  l$attrs <- lapply(l$attrs, function(x) structure(x, class = "plotly_eval"))
  l$attrs[[1]][["type"]] <- "ggplotly"
  l$cur_data <- ids[[layerData]]
  l$visdat <- setNames(lapply(return_dat, function(x) function(y) x), 
                       ids)
  l
}
