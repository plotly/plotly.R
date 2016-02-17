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
#' # maps!!
#' data(canada.cities, package = "maps")
#' viz <- ggplot(canada.cities, aes(long, lat)) +
#'   borders(regions = "canada", name = "borders") +
#'   coord_equal() +
#'   geom_point(aes(text = name, size = pop), colour = "red",
#'                alpha = 1/2, name = "cities")
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
  # for some geoms (e.g. boxplots) it's better to send the 
  # "pre-statistics" data and let plotly compute statistics
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
  # end of ggplot_build(), start of layer -> trace conversion
  # ------------------------------------------------------------------------
  
  nPanels <- nrow(panel$layout)
  nRows <- max(panel$layout$ROW)
  nCols <- max(panel$layout$COL)
  
  # for consistency in the layout data structure across grid/wrap
  if (!inherits(p$facet, "wrap")) {
    panel$layout$AXIS_X <- panel$layout$ROW == nRows
    panel$layout$AXIS_Y <- panel$layout$COL == 1
  }
  # panel -> plotly.js axis info
  panel$layout$xaxis <- with(panel$layout, ifelse(AXIS_X, PANEL, COL))
  panel$layout$xaxis <- paste0("xaxis", sub("1", "", panel$layout$xaxis))
  panel$layout$yaxis <- with(panel$layout, ifelse(AXIS_Y, PANEL, ROW))
  panel$layout$yaxis <- paste0("yaxis", sub("1", "", panel$layout$yaxis))
  # panel -> plotly.js axis anchor info
  panel$layout$xanchor <- with(panel$layout, ifelse(AXIS_X, PANEL, nRows))
  panel$layout$xanchor <- paste0("y", sub("1", "", panel$layout$xanchor))
  panel$layout$yanchor <- with(panel$layout, ifelse(AXIS_Y, PANEL, 1))
  panel$layout$yanchor <- paste0("x", sub("1", "", panel$layout$yanchor))
  
  # merge the panel/axis info with each _layer_ of data
  lay_out <- panel$layout[c("PANEL", "xaxis", "yaxis")]
  lay_out$xaxis <- sub("axis", "", lay_out$xaxis)
  lay_out$yaxis <- sub("axis", "", lay_out$yaxis)
  lay_outs <- vector("list", length(data))
  for (i in seq_len(length(data))) {
    lay_outs[[i]] <- lay_out
  }
  data <- Map(function(x, y) { merge(x, y, sort = FALSE) }, data, lay_outs)
  
  # layers -> plotly.js traces
  trace.list <- layers2traces(data, prestats_data, layers)
  # collapse lists of lists to a list of traces 
  # TODO: attach the appropriate panel/axis/legendgroup info to each trace!
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
  
  # obtain _calculated_ theme elements (necessary for axis/label formatting)
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
    # global font (all other font objects inherit from it)
    font = text2font(theme$text)
  )
  # main plot title
  if (nchar(p$labels$title %||% "") > 0) {
    gglayout$title <- faced(p$labels$title, theme$plot.title$face)
    gglayout$titlefont <- text2font(theme$plot.title)
    # TODO: account for theme$plot.margin$margin here as well?
    gglayout$margin$t <- gglayout$margin$t + gglayout$titlefont$size
  }
  
  # domains are actually dynamically computed (see inst/htmlwidgets/plotly.js)
  # this assumes no margins (sorry plotly_POST()) but we'll adjust later
  doms <- get_domains(nPanels, nRows, 0)
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
      # TODO: 
      # (1) We currently ignore minor grid lines. Would it makes sense to 
      # create another axis object just for the minor grid?
      # (2) translate panel.border to a rect shape
      # (3) Does it make sense to translate other theme elements?
      axisName <- lay[, paste0(xy, "axis")]
      anchor <- lay[, paste0(xy, "anchor")]
      rng <- panel$ranges[[i]]
      
      sc <- scales$get_scales(xy)
      # set some axis defaults (and override some of them later)
      # https://plot.ly/r/reference/#layout-xaxis
      axisObj <- list(
        title = if (!is.null(axisTitle)) sc$name %||% p$labels[[xy]],
        titlefont = text2font(axisTitle),
        type = "linear",
        autorange = FALSE,
        tickmode = "array",
        range = rng[[paste0(xy, ".range")]],
        ticktext = rng[[paste0(xy, ".labels")]],
        tickvals = rng[[paste0(xy, ".major")]],
        ticks = if (is_blank(axisTicks)) "" else "outside",
        tickcolor = toRGB(axisTicks$colour),
        ticklen = unitConvert(theme$axis.ticks.length, "pixels", "height"),
        tickwidth = unitConvert(axisTicks$size, "pixels", "width"),
        showticklabels = TRUE,
        tickfont = text2font(axisText),
        tickangle = -axisText$angle,
        showline = !is_blank(axisLine),
        linecolor = toRGB(axisLine$colour),
        # TODO: convert this size? Is this in points?
        linewidth = unitConvert(axisLine$size, "pixels", "width"),
        showgrid = !is_blank(panelGrid),
        domain = sort(as.numeric(doms[i, paste0(xy, c("start", "end"))])),
        gridcolor = toRGB(panelGrid$colour),
        # TODO: convert this size? Is this in points?
        gridwidth = panelGrid$size,
        zeroline = FALSE,  # ggplot2 never shows zero lines
        anchor = anchor
      )
      # bold/italic axis title
      axisObj$title <- faced(axisObj$title, theme$axis.text$face)
      axisObj <- re_scale(axisObj, sc)
      
      # tack axis object onto the layout
      gglayout[[axisName]] <- axisObj
      
      # account for axis title/text in plot margins
      if (i == 1) {
        side <- if (xy == "x") "b" else "l"
        type <- if (xy == "x") "height" else "width"
        # just support _bottom_ x-axis text & _left_ y-axis margins
        # (plotly.js has no sense of padding between ticks and ticktext)
        idx <- if (xy == "x") 3 else 4
        gglayout$margin[[side]] <- gglayout$margin[[side]] + 
          unitConvert(axisTitle$margin %||% rep(0, 4), "pixels")[idx] + 
          unitConvert(axisText$margin %||% rep(0, 4), "pixels")[idx] + 
          unitConvert(axisText$size, "pixels", type) +
          unitConvert(axisTicks$size, "pixels", type)
          # nice idea, but not right (yet)
          #with(axisObj, bbox(ticktext, tickangle, tickfont$size))[[way]] +
      }
      
    } # axis loop
    xdom <- gglayout[[lay[, "xaxis"]]]$domain
    ydom <- gglayout[[lay[, "yaxis"]]]$domain
    
    # facet strips -> plotly annotations
    # TODO: 
    # (1) use p$facet$labeller for the actual strip text!
    if (inherits(p$facet, "grid") && lay$COL == nCols) {
      txt <- paste(
        lay[, as.character(p$facet$rows)], 
        collapse = ", "
      )
      lab <- make_label(
        txt, x = mean(xdom), y = mean(ydom), 
        el = theme[["strip.text.y"]] %||% theme[["strip.text"]],
        xanchor = "center", yanchor = "bottom"
      )
      gglayout$annotations <- c(gglayout$annotations, lab)
      # draw the strip label as a rect shape
      strip <- make_strip_rect(xdom, ydom, theme, "right")
      gglayout$shapes <- c(gglayout$shapes, strip)
    }
    
    if (inherits(p$facet, "wrap") || 
        inherits(p$facet, "grid") && lay$ROW == 1) {
      vars <- ifelse(inherits(p$facet, "wrap"), "facets", "cols")
      txt <- paste(
        lay[, as.character(p$facet[[vars]])], 
        collapse = ", "
      )
      lab <- make_label(
        txt, x = mean(xdom), y = max(ydom), 
        el = theme[["strip.text.x"]] %||% theme[["strip.text"]],
        xanchor = "center", yanchor = "bottom"
      )
      gglayout$annotations <- c(gglayout$annotations, lab)
      # draw the strip label as a rect shape
      strip <- make_strip_rect(xdom, ydom, theme, "top")
      gglayout$shapes <- c(gglayout$shapes, strip)
    }
  }   # panel loop
  
  if (has_facet(p)) {
    # space for the first row of strip text goes in plot margin 
    # (other rows are accounted for in the axes domains)
    xStripText <- theme[["strip.text.x"]] %||% theme[["strip.text"]]
    yStripText <- theme[["strip.text.y"]] %||% theme[["strip.text"]]
    gglayout$margin$t <- gglayout$margin$t + unitConvert(xStripText$size, "pixels", "height")
    if (inherits(p$facets, "grid")) {
      gglayout$margin$r <- gglayout$margin$r + unitConvert(xStripText$size, "pixels", "height")
    }
    # each plot should only have _one_ axis title
    xAxisTitle <- theme[["axis.title.x"]] %||% theme[["axis.title"]]
    yAxisTitle <- theme[["axis.title.y"]] %||% theme[["axis.title"]]
    gglayout$annotations <- c(
      gglayout$annotations, 
      make_label(
        gglayout$xaxis$title, 
        x = 0.5, 
        y = 0 - unitConvert(gglayout$xaxis$titlefont$size, "npc", "height"),
        el = xAxisTitle, 
        yanchor = "bottom"
      ),
      make_label(
        gglayout$yaxis$title, 
        x = 0 - unitConvert(gglayout$yaxis$titlefont$size, "npc", "width"), 
        y = 0.5, 
        el = yAxisTitle, 
        xanchor = "bottom"
      )
    )
    gglayout <- strip_axis(gglayout, "title")
  }
  
  # ------------------------------------------------------------------------
  # guide/legend conversion
  # ------------------------------------------------------------------------ 
  
  # if there are no non-positional scales or if theme(legend.position = "none")
  # is used, don't show a legend at all.
  if (npscales$n() == 0 || identical(theme$legend.position, "none")) {
    gglayout$showlegend  <- FALSE
  } else {
    # Strategy: Obtain and translate the output of ggplot2:::guides_train()
    # To do so, we copy some of the body of ggplot2:::guides_build()
    
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
        # should always be numeric (it's a colorbar!)
        gdef$key$.value <- scales::rescale(gdef$key$.value)
        return(list(
          x = gglayout$xaxis$tickvals,
          y = gglayout$yaxis$tickvals,
          type = "scatter",
          mode = "markers",
          opacity = 0,
          hoverinfo = "none",
          showlegend = FALSE,
          marker = list(
            color = gdef$key$.value,
            colorscale = setNames(gdef$key[c(".value", "fill")], NULL),
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
        # str(gdef$geoms)
        # problem: multiple geoms can belong to a single legend. 
        # How to convert multiple geoms to a single trace? Just take the first?
        NULL
      }
      return(NULL)
    }
    
    traces <- c(traces, lapply(gdefs, gdef2trace))
    
    # TODO: 
    # (1) shrink guide size(s). Set fractions in colorbar.lenmode
    # (2) position guide(s)?
    # (3) 
    
  }
  
  
  #gglayout$legend <- list(
  #  bordercolor = "transparent", 
  #  bgcolor = toRGB(theme$legend.background$fill),
  #  x = 1.01, 
  #  y = 0.075 * 0.5* length(trace.list) + 0.45,
  #  xref="paper", yref="paper",
  #  # these should always be "center"?
  #  xanchor = "center", 
  #  yanchor = "center",
  #  font = text2font(theme$legend.text)
  #)
  
  #mode.mat <- matrix(NA, 3, 3)
  #rownames(mode.mat) <- colnames(mode.mat) <- c("markers", "lines", "none")
  #mode.mat["markers", "lines"] <-
  #  mode.mat["lines", "markers"] <- "lines+markers"
  #mode.mat["markers", "none"] <- mode.mat["none", "markers"] <- "markers"
  #mode.mat["lines", "none"] <- mode.mat["none", "lines"] <- "lines"
  #merged.traces <- list()
  #not.merged <- trace.list
  #while(length(not.merged)){
  #  tr <- not.merged[[1]]
  #  not.merged <- not.merged[-1]
  #  # Are there any traces that have not yet been merged, and can be
  #  # merged with tr?
  #  can.merge <- logical(length(not.merged))
  #  for(other.i in seq_along(not.merged)){
  #    other <- not.merged[[other.i]]
  #    criteria <- c()
  #    for(must.be.equal in c("x", "y", "xaxis", "yaxis")){
  #      other.attr <- other[[must.be.equal]]
  #      tr.attr <- tr[[must.be.equal]]
  #      criteria[[must.be.equal]] <- 
  #        isTRUE(all.equal(other.attr, tr.attr)) && 
  #        unique(other$type, tr$type) == "scatter"
  #    }
  #    if(all(criteria)){
  #      can.merge[[other.i]] <- TRUE
  #    }
  #  }
  #  to.merge <- not.merged[can.merge]
  #  not.merged <- not.merged[!can.merge]
  #  for(other in to.merge){
  #    new.mode <- tryCatch({
  #      mode.mat[tr$mode, other$mode]
  #    }, error=function(e){
  #      NA
  #    })
  #    if(is.character(new.mode) && !is.na(new.mode %||% NA)){
  #      tr$mode <- new.mode
  #    }
  #    attrs <- c("error_x", "error_y", "marker", "line")
  #    for(attr in attrs){
  #      if(!is.null(other[[attr]]) && is.null(tr[[attr]])){
  #        tr[[attr]] <- other[[attr]]
  #      }
  #    }
  #  }
  #  merged.traces[[length(merged.traces)+1]] <- tr
  #}
  
  
  # add space for axis/facet strip text
  # TODO: move this up?
  yAxisText <- theme[["axis.text.y"]] %||% theme[["axis.text"]]
  xAxisText <- theme[["axis.text.x"]] %||% theme[["axis.text"]]
  stripText <- theme[["strip.text.x"]] %||% theme[["strip.text"]]
  panel$layout$tMargin <- ifelse(
    panel$layout$ROW > 1 & inherits(p$facet, "wrap"), 
    unitConvert(stripText$size, "pixels", "height") * 2.1, 
    0
  )
  panel$layout$rMargin <- ifelse(
    panel$layout$COL < nCols & isTRUE(p$facet$free$y),
    unitConvert(yAxisText$size, "pixels", "height") * 1.5, 
    0
  )
  panel$layout$bMargin <- ifelse(
    panel$layout$ROW < nRows & isTRUE(p$facet$free$x),
    unitConvert(xAxisText$size, "pixels", "height") * 2.1,
    0
  )
  panel$layout$lMargin <- ifelse(
    panel$layout$COL > 1 & isTRUE(p$facet$free$y), 
    unitConvert(yAxisText$size, "pixels", "height") * 1.5, 
    0
  )
  # note that `layout.axisid.margins` is not a plotly.js property
  # but is used in the HTMLwidget.resize() method for dynamic resizing
  # the margins are measured in pixels here, but are normalized to a 0-1
  # scale based on the height/width of the client window.
  for (i in seq_len(nPanels)) {
    lay <- panel$layout[i, ]
    gglayout[[lay$xaxis]]$margins <- c(lay$lMargin, lay$rMargin) +
      unitConvert(theme$panel.margin.x %||% theme$panel.margin, "pixels")
    gglayout[[lay$yaxis]]$margins <- c(lay$bMargin, lay$tMargin) +
      unitConvert(theme$panel.margin.y %||% theme$panel.margin, "pixels")
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

# aesthetics that should have a trace for each unique value
markSplit <- list(
  GeomBoxplot = "x",
  GeomPoint = c("colour", "fill", "shape"),
  GeomPath = c("linetype", "size", "colour", "shape"),
  GeomPolygon = c("colour", "fill", "linetype", "size"),
  GeomBar = c("colour", "fill"),
  GeomDensity = c("colour", "fill", "linetype"),
  GeomBoxplot = c("colour", "fill", "size"),
  GeomErrorbar = c("colour", "linetype"),
  GeomErrorbarh = c("colour", "linetype"),
  GeomArea = c("colour", "fill"),
  GeomStep = c("linetype", "size", "colour"),
  GeomText = c("colour")
)

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
    u[1] <- grid::convertHeight(u[1], "npc")
    u[2] <- grid::convertWidth(u[2], "npc")
    u[3] <- grid::convertHeight(u[3], "npc")
    u[4] <- grid::convertWidth(u[4], "npc")
  } else {
    u <- convert(u, "npc")
  }
  
  if (to[1] == "pixels") {
    if (inherits(u, "margin")) {
      u[1] <- mm2pixels(grid::convertHeight(u[1], "mm"))
      u[2] <- mm2pixels(grid::convertWidth(u[2], "mm"))
      u[3] <- mm2pixels(grid::convertHeight(u[3], "mm"))
      u[4] <- mm2pixels(grid::convertWidth(u[4], "mm"))
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
  u <- u %||% 0
  # the default unit in ggplot2 is millimeters
  if (is.null(attr(u, "unit"))) {
    u <- grid::unit(u, "mm")
  }
  u
}


# detect a blank theme element
is_blank <- function(x) {
  inherits(x, "element_blank") && inherits(x, "element")
}

# obtain the "type" of geom/position/etc.
type <- function(x, y) {
  sub(y, "", tolower(class(x[[y]])[[1]]))
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

# remove a property from an axis element
strip_axis <- function(x, y = "title") {
  idx <- grepl("[x-y]axis", names(x))
  axes <- x[idx]
  axes <- lapply(axes, function(x) { x[[y]] <- NULL; x })
  x[idx] <- axes
  x
}

#' Conservative estimate of height/width of a string
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
  # first, compute the hypotenus
  hyp <- sqrt(size ^ 2 + w ^ 2)
  list(
    v = max(hyp * cos(90 - angle), size),
    h = max(hyp * sin(90 - angle), w)
  )
}

# create a plotly font object from ggplot2::element_text() 
text2font <- function(x = ggplot2::element_text()) {
  list(
    color = toRGB(x$colour),
    family = x$family,
    size = unitConvert(grid::unit(x$size %||% 0, "points"), "pixels", "height")
  )
}

# wrap text in bold/italics according to the text "face"
faced <- function(txt, face) {
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
  cl <- class(scale)
  if ("ScaleDiscrete" %in% cl) {
    axisObj$type <- "category"
    axisObj$range <- axisObj$ticktext
    axisObj$autorange <- TRUE
    axisObj$tickvals <- NULL
  } else {
    # these tick locations are always on a 0-1 scale, but we want them on the
    # data scale
    axisObj$tickvals <- scales::rescale(
      axisObj$tickvals, to = axisObj$range, from = c(0, 1)
    )
  }
  axisObj
}

to_milliseconds <- function(x, warn = FALSE) {
  if (inherits(x, "POSIXt")) {
    # Convert seconds into milliseconds
    x <- as.numeric(x) * 1000
  } else if (inherits(x, "Date")) {
    # Convert days into milliseconds
    x <- as.numeric(x) * 24 * 60 * 60 * 1000
  } else {
    if (warn) warning("Expected a date or datetime")
  }
  x
}

# if a vector has one unique value, return that value
uniq <- function(x) {
  u <- unique(x)
  if (length(u) == 1) u else x
}

# We need access to internal ggplot2 functions in several places
# this helps us import functions in a way that R CMD check won't cry about
ggfun <- function(x) getFromNamespace(x, "ggplot2")

make_strip_rect <- function(xdom, ydom, theme, side = "top") {
  rekt <- list(
    type = "rect",
    fillcolor = toRGB(theme[["strip.background"]]$fill),
    line = list(
      color = toRGB(theme[["strip.background"]]$colour),
      width = unitConvert(theme[["strip.background"]]$size, "pixels", "height"),
      linetype = lty2dash(theme[["strip.background"]]$linetype)
    ),
    yref = "paper",
    xref = "paper"
  )
  stripTextX <- theme[["strip.text.x"]] %||% theme[["strip.text"]]
  stripTextX <- unitConvert(stripTextX$size, "npc", "width")
  stripTextY <- theme[["strip.text.y"]] %||% theme[["strip.text"]]
  stripTextY <- unitConvert(stripTextY$size, "npc", "height")
  panelMarginX <- theme[["panel.margin.x"]] %||% theme[["panel.margin"]]
  panelMarginX <- unitConvert(panelMarginX, "npc", "width")
  panelMarginY <- theme[["panel.margin.y"]] %||% theme[["panel.margin"]]
  panelMarginY <- unitConvert(panelMarginY, "npc", "height")
  if ("right" %in% side) {
    rekt$x0 <- xdom[2] 
    rekt$x1 <- xdom[2] 
    rekt$y0 <- ydom[1]
    rekt$y1 <- ydom[2]
  }
  if ("top" %in% side) {
    rekt$x0 <- xdom[1] + panelMarginX
    rekt$x1 <- xdom[2] - panelMarginX
    rekt$y0 <- ydom[2] - panelMarginY
    rekt$y1 <- ydom[2] + panelMarginY + stripTextY
  }
  list(rekt)
}
