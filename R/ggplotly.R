#' Create plotly graphs using ggplot2 syntax
#'
#' See up-to-date documentation and examples at
#' \url{https://plot.ly/ggplot2}
#'
#' @param p a ggplot object.
#' @param width	Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
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
#'  ggplotly(viz)
#' }
#' 
ggplotly <- function(p = ggplot2::last_plot(), width = NULL, height = NULL) {
  l <- gg2list(p, width = width, height = height)
  hash_plot(p$data, l)
}

#' Convert a ggplot to a list.
#' @import ggplot2
#' @param p ggplot2 plot.
#' @param width	Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @return a 'built' plotly object (list with names "data" and "layout").
#' @export
gg2list <- function(p, width = NULL, height = NULL) {
  # ------------------------------------------------------------------------
  # Our internal version of ggplot2::ggplot_build(). Modified from
  # https://github.com/hadley/ggplot2/blob/0cd0ba/R/plot-build.r#L18-L92
  # ------------------------------------------------------------------------
  p <- ggfun("plot_clone")(p)
  if (length(p$layers) == 0) {
    p <- p + geom_blank()
  }
  layers <- p$layers
  layer_data <- lapply(layers, function(y) y$layer_data(p$data))
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
  data <- by_layer(function(l, d) l$compute_aesthetics(d, p))
  data <- lapply(data, ggfun("scales_transform_df"), scales = scales)
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  panel <- ggfun("train_position")(panel, data, scale_x(), scale_y())
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
    }
    if (p$facet$free$x && p$facet$free$y) {
      panel$layout$xaxis <- panel$layout$PANEL
      panel$layout$yaxis <- panel$layout$PANEL
      panel$layout$xanchor <- panel$layout$PANEL
      panel$layout$yanchor <- panel$layout$PANEL
    }
  }
  # format the axis/anchor to a format plotly.js respects
  panel$layout$xaxis <- paste0("xaxis", sub("1", "", panel$layout$xaxis))
  panel$layout$yaxis <- paste0("yaxis", sub("1", "", panel$layout$yaxis))
  panel$layout$xanchor <- paste0("y", sub("1", "", panel$layout$xanchor))
  panel$layout$yanchor <- paste0("x", sub("1", "", panel$layout$yanchor))
  
  # merge the panel/axis info with _each layer_ of data so we know where to put
  # each trace
  lay_out <- panel$layout[c("PANEL", "xaxis", "yaxis")]
  lay_out$xaxis <- sub("axis", "", lay_out$xaxis)
  lay_out$yaxis <- sub("axis", "", lay_out$yaxis)
  lay_outs <- vector("list", length(data))
  for (i in seq_len(length(data))) {
    lay_outs[[i]] <- lay_out
  }
  data <- Map(function(x, y) { merge(x, y, sort = FALSE) }, data, lay_outs)
  # layers -> plotly.js traces
  trace.list <- layers2traces(data, prestats_data, layers, scales)
  # collapse lists of lists to a list of traces 
  # TODO: attach the appropriate legendgroup info to each trace!
  traces <- list()
  for (i in seq_along(trace.list)) {
    traces <- c(traces, trace.list[[i]])
  }
  
  # don't show legends that are automatically created from these traces
  # later on, when legends/guides are created, 
  # we may tack on more traces with visible="legendonly"
  traces <- lapply(traces, function(x) { x$showlegend <- FALSE; x})

  
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
    # TODO: why does stripSize need to be inflated here?
    panelMarginY <- panelMarginY + 1.5 * stripSize
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
        bbox(lab, axisTextX$angle, unitConvert(axisTextX, "npc", "height"))$v
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
        bbox(lab, axisTextY$angle, unitConvert(axisTextY, "npc", "width"))$h
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
      # https://plot.ly/r/reference/#layout-xaxis
      axisObj <- list(
        # this might be changed later in re_scale()
        type = "linear",
        autorange = FALSE,
        tickmode = "array",
        range = rng[[paste0(xy, ".range")]],
        ticktext = rng[[paste0(xy, ".labels")]],
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
        anchor = anchor
      )
      # TODO: implement minor grid lines with another axis object 
      # and _always_ hide ticks/text?
      gglayout[[axisName]] <- re_scale(axisObj, sc)
      
      # do some stuff that should be done once for the entire plot
      if (i == 1) {
        # convert days to milliseconds, if necessary
        if ("date" %in% sc$scale_name) {
          traces <- lapply(traces, function(z) { 
            z[[xy]] <- z[[xy]] * 24 * 60 * 60 * 1000
            z
          })
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
        }
        axisTitleText <- sc$name %||% p$labels[[xy]] %||% ""
        axisTickText <- axisObj$ticktext[which.max(nchar(axisObj$ticktext))]
        side <- if (xy == "x") "b" else "l"
        way <- if (xy == "x") "v" else "h"
        # account for axis ticks, ticks text, and titles in plot margins
        # (apparently ggplot2 doesn't support axis.title/axis.text margins)
        gglayout$margin[[side]] <- gglayout$margin[[side]] + axisObj$ticklen +
          bbox(axisTickText, axisObj$tickangle, axisObj$tickfont$size)[[way]] +
          bbox(axisTitleText, axisTitle$angle, unitConvert(axisTitle, "pixels", type))[[way]]
        # draw axis titles as annotations 
        # (plotly.js axis titles aren't smart enough to dodge ticks & text)
        if (!is_blank(axisTitle) && nchar(axisTitleText) > 0) {
          axisTextSize <- unitConvert(axisText, "npc", type)
          axisTitleSize <- unitConvert(axisTitle, "npc", type)
          offset <- 
            (0 - 
               bbox(axisTickText, axisText$angle, axisTextSize)[[way]] -
               bbox(axisTitleText, axisTitle$angle, axisTitleSize)[[way]] / 2 -
               unitConvert(theme$axis.ticks.length, "npc", type))
          # npc is on a 0-1 scale of the _entire_ device, 
          # but these units _should_ be wrt to the plotting region
          # multiplying the offset by 2 seems to work, but this is a terrible hack
          offset <- 2 * offset
          x <- if (xy == "x") 0.5 else offset
          y <- if (xy == "x") offset else 0.5
          gglayout$annotations <- c(
            gglayout$annotations,
            make_label(
              faced(axisTitleText, axisTitle$face), x, y, el = axisTitle, 
              xanchor = "center", yanchor = "middle"
            )
          )
        }
      }
      
    } # end of axis loop
    
    # draw panel border
    xdom <- gglayout[[lay[, "xaxis"]]]$domain
    ydom <- gglayout[[lay[, "yaxis"]]]$domain
    border <- make_panel_border(xdom, ydom, theme)
    gglayout$shapes <- c(gglayout$shapes, border)
    
    # facet strips -> plotly annotations
    # TODO: use p$facet$labeller for the actual strip text!
    if (has_facet(p) && lay$ROW == 1 && !is_blank(theme[["strip.text.x"]])){
      vars <- ifelse(inherits(p$facet, "wrap"), "facets", "cols")
      txt <- paste(
        lay[, as.character(p$facet[[vars]])], collapse = ", "
      )
      lab <- make_label(
        txt, x = mean(xdom), y = max(ydom), 
        el = theme[["strip.text.x"]] %||% theme[["strip.text"]],
        xanchor = "center", yanchor = "bottom"
      )
      gglayout$annotations <- c(gglayout$annotations, lab)
      strip <- make_strip_rect(xdom, ydom, theme, "top")
      gglayout$shapes <- c(gglayout$shapes, strip)
    }
    if (inherits(p$facet, "grid") && lay$COL == nCols && 
        !is_blank(theme[["strip.text.y"]])) {
      txt <- paste(
        lay[, as.character(p$facet$rows)], collapse = ", "
      )
      lab <- make_label(
        txt, x = max(xdom), y = mean(ydom), 
        el = theme[["strip.text.y"]] %||% theme[["strip.text"]],
        xanchor = "left", yanchor = "bottom"
      )
      gglayout$annotations <- c(gglayout$annotations, lab)
      strip <- make_strip_rect(xdom, ydom, theme, "right")
      gglayout$shapes <- c(gglayout$shapes, strip)
    }
    
  } # end of panel loop
  
  
  # ------------------------------------------------------------------------
  # guide/legend conversion
  #   Strategy: Obtain and translate the output of ggplot2:::guides_train().
  #   To do so, we borrow some of the body of ggplot2:::guides_build().
  #
  #   Once we have legend key(s). We use those keys to decide when/where to 
  #   split layers into plotly.js traces. We also use the key name 
  #   (e.g. fill, color, etc) to set the legendgroup property and the key
  #   values to name the traces.
  # ------------------------------------------------------------------------ 
  
  # if there are no non-positional scales or if theme(legend.position = "none")
  # is used, don't show a legend at all.
  if (npscales$n() == 0 || identical(theme$legend.position, "none")) {
    gglayout$showlegend  <- FALSE
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
    gdefs <- ggfun("guides_merge")(gdefs)
    gdefs <- ggfun("guides_geom")(gdefs, layers, p$mapping)
    
    # guide data -> plotly.js traces
    gdef2trace <- function(gdef) {
      if (inherits(gdef, "colorbar")) {
        # sometimes the key has missing values, which we can ignore
        gdef$key <- gdef$key[!is.na(gdef$key$.value), ]
        # range of the scale
        rng <- range(gdef$bar$value)
        gdef$bar$value <- scales::rescale(gdef$bar$value, from = rng)
        gdef$key$.value <- scales::rescale(gdef$key$.value, from = rng)
        return(list(
          x = gglayout$xaxis$tickvals,
          y = gglayout$yaxis$tickvals,
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
              title = gdef$title,
              titlefont = text2font(gdef$title.theme),
              tickmode = "array",
              ticktext = gdef$key$.label,
              tickvals = gdef$key$.value
            )
          )
        ))
      }
      # TODO: convert legends. Connect with traces via legendgroup!
      if (inherits(gdef, "legend")) {
        # unfortunately we have no nice way to identify the geom type of gdef$geoms
        # note: multiple geoms can belong to a single legend.
        
        # How to convert multiple geoms to a single trace? Just take the first?
        NULL
      }
      return(NULL)
    }
    traces <- c(traces, lapply(gdefs, gdef2trace))
    
    # TODO: 
    # (1) shrink guide size(s). Set fractions in colorbar.lenmode
    # (2) position guide(s)?
  }
  
  
  
  
  
  
  
  
  # --------
  # plot-wide hacks 
  # ---------
  
  # Bar hackery:
  # (1) coord_flip() is plot-specific, but `bar.orientiation` is trace-specific 
  # (2) position_*() is layer-specific, but `layout.barmode` is plot-specific.
  geoms <- sapply(layers, ggtype, "geom")
  if (any(idx <- geoms %in% "bar")) {
    # note: ggplot2 doesn't flip x/y scales when the coord is flipped
    # (i.e., at this point, y should be the count/density)
    is_hist <- inherits(p$scales$get_scales("x"), "ScaleContinuous")
    # TODO: get rid of this and use explicit width for bars 
    # https://github.com/plotly/plotly.js/issues/80
    if (is_hist) gglayout$bargap <- 0
    # since `layout.barmode` is plot-specific, we can't support multiple bar 
    # geoms with different positions
    positions <- sapply(layers, ggtype, "position")
    position <- unique(positions[geoms %in% "bar"])
    if (length(position) > 1) {
      warning("plotly doesn't support multiple positions\n",
              "across geom_bar() layers", call. = FALSE)
      position <- position[1]
    }
    gglayout$barmode <- if (position %in% "identity" && is_hist) {
      "overlay" 
    } else if (position %in% c("identity", "stack", "fill")) {
      "stack"
    } else {
      "group"
    }
  }
  
  # flip x/y in traces for flipped coordinates 
  # (we've already done appropriate flipping for axis objects)
  if (inherits(p$coordinates, "CoordFlip")) {
    for (i in seq_along(traces)) {
      tr <- traces[[i]]
      traces[[i]][c("x", "y")] <- tr[c("y", "x")]
      if (tr$type %in% c("bar", "box")) traces[[i]]$orientation <- "h"
    }
  }
  
  l <- list(data = compact(traces), layout = compact(gglayout))
  # ensure properties are boxed correctly
  l <- add_boxed(rm_asis(l))
  l$width <- width
  l$height <- height
  structure(l, class = "plotly")
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
  w <- size * (nchar(txt) / 2)
  # do the sensible thing in the majority of cases
  if (angle == 0) return(list(v = size, h = w))
  if (abs(angle) == 90) return(list(v = w, h = size))
  # first, compute the hypotenus
  hyp <- sqrt(size ^ 2 + w ^ 2)
  list(
    v = max(hyp * cos(90 - angle), size),
    h = max(hyp * sin(90 - angle), w)
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


re_scale <- function(axisObj, scale) {
  # hopefully scale_name doesn't go away (otherwise, we might have to determine
  # date vs datetime from the raw data)
  # https://github.com/hadley/ggplot2/issues/1312
  if ("date" %in% scale$scale_name || "datetime" %in% scale$scale_name) {
    axisObj$type <- "date"
    # convert dates to milliseconds (so everything is a datetime)
    if ("date" %in% scale$scale_name) {
      axisObj$range <- axisObj$range * 24 * 60 * 60 * 1000
    }
  }
  axisObj$tickvals <- scales::rescale(
    axisObj$tickvals, to = axisObj$range, from = c(0, 1)
  )
  axisObj
}

# if a vector has one unique value, return that value
uniq <- function(x) {
  u <- unique(x)
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
ggfun <- function(x) getFromNamespace(x, "ggplot2")

ggtype <- function(x, y = "geom") {
  sub(y, "", tolower(class(x[[y]])[1]))
}

