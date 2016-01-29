#' Create plotly graphs using ggplot2 syntax
#'
#' See up-to-date documentation and examples at
#' \url{https://plot.ly/ggplot2}
#'
#' @param p a ggplot object.
#' @param filename character string describing the name of the plot in your plotly account. 
#' Use / to specify directories. If a directory path does not exist it will be created.
#' If this argument is not specified and the title of the plot exists,
#' that will be used for the filename.
#' @param fileopt character string describing whether to create a "new" plotly, "overwrite" an existing plotly, 
#' "append" data to existing plotly, or "extend" it.
#' @param world_readable logical. If \code{TRUE}, the graph is viewable 
#' by anyone who has the link and in the owner's plotly account.
#' If \code{FALSE}, graph is only viewable in the owner's plotly account.
#' @seealso \link{signup}, \link{plot_ly}
#' @import httr jsonlite
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
ggplotly <- function(p = ggplot2::last_plot(), filename, fileopt, 
                     world_readable = TRUE) {
  l <- gg2list(p)
  # tack on special keyword arguments
  if (!missing(filename)) l$filename <- filename
  if (!missing(fileopt)) l$fileopt <- fileopt
  l$world_readable <- world_readable
  hash_plot(p$data, l)
}

# ----------------------------------------------------------------------------
# Objects accessed inside gg2list()
# ----------------------------------------------------------------------------

# calc. the epoch
now <- Sys.time()
the.epoch <- now - as.numeric(now)

# detect a blank theme element
is_blank <- function(x) {
  inherits(x, "element_blank") && inherits(x, "element")
}

# ggplot2 size is in millimeters. plotly is in pixels. To do this correctly, 
# we need to know PPI/DPI of the display. I'm not sure of a decent way to do that
# from R, but it seems 96 is a reasonable assumption.
mm2pixels <- function(mm) { 
  (mm * 96) / 25.4 
}
# convert an aribitrary grid unit to pixels
unit2pixels <- function(u) {
  if (!length(u)) return(0)
  # most ggplot2 sizes seem to use points
  if (is.null(attr(u, "unit"))) u <- grid::unit(u, "points")
  mm2pixels(as.numeric(grid::convertUnit(u, "mm")))
}
# TODO: the correct way to convert sizes would be to send 'npc' (0, 1)
# units to the browser & use HTMLwidget's resize method to scale everything
# to the display. Only problem is that this would require extensive knowledge
# of things that need resizing 

aesConverters <- list(
  linetype=function(lty) {
    lty2dash[as.character(lty)]
  },
  colour=function(col) {
    toRGB(col)
  },
  size=mm2pixels,
  sizeref=identity,
  sizemode=identity,
  alpha=identity,
  shape=function(pch) {
    pch2symbol[as.character(pch)]
  },
  direction=identity
)

markLegends <-
  # NOTE: Do we also want to split on size?
  # Legends based on sizes not implemented yet in Plotly
  #  list(point=c("colour", "fill", "shape", "size"),
  list(point=c("colour", "fill", "shape"),
       path=c("linetype", "size", "colour", "shape"),
       ## NOTE: typically "group" should not be present here, since
       ## that would mean creating a separate plotly legend for each
       ## group, even when they have the exact same visual
       ## characteristics and could be drawn using just 1 trace!
       polygon=c("colour", "fill", "linetype", "size"),
       bar=c("colour", "fill"),
       density=c("colour", "fill", "linetype"),
       boxplot=c("colour", "fill", "size"),
       errorbar=c("colour", "linetype"),
       errorbarh=c("colour", "linetype"),
       area=c("colour", "fill"),
       step=c("linetype", "size", "colour"),
       text=c("colour"))

markUnique <- as.character(unique(unlist(markLegends)))

markSplit <- markLegends
markSplit$boxplot <- "x"

# obtain the "type" of geom/position/etc.
type <- function(x, y) {
  sub(y, "", tolower(class(x[[y]])[[1]]))
}

guide_names <- function(p, aes = c("shape", "fill", "alpha", "area",
                                   "color", "colour", "size", "linetype")) {
  sc <- as.list(p$scales)$scales
  nms <- lapply(sc, "[[", "name")
  if (length(nms) > 0) {
    names(nms) <- lapply(sc, "[[", "aesthetics")
    if (is.null(unlist(nms))) {nms <- list()}
  }
  unlist(modifyList(p$labels[names(p$labels) %in% aes], nms))
}

#' Convert a ggplot to a list.
#' @import ggplot2
#' @param p ggplot2 plot.
#' @return figure object (list with names "data" and "layout").
#' @export
gg2list <- function(p) {
  # ggplot now applies geom_blank() (instead of erroring) when no layers exist
  if (length(p$layers) == 0) p <- p + geom_blank()
  layout <- list()
  trace.list <- list()
  
  # Before building the ggplot, we would like to add aes(name) to
  # figure out what the object group is later. This also copies any
  # needed global aes/data values to each layer, so we do not have to
  # worry about combining global and layer-specific aes/data later.
  for(layer.i in seq_along(p$layers)) {
    layer.aes <- p$layers[[layer.i]]$mapping
    if(p$layers[[layer.i]]$inherit.aes){
      to.copy <- names(p$mapping)[!names(p$mapping) %in% names(layer.aes)]
      layer.aes[to.copy] <- p$mapping[to.copy]
    }
    mark.names <- names(layer.aes) # make aes.name for all aes.
    name.names <- sprintf("%s.name", mark.names)
    layer.aes[name.names] <- layer.aes[mark.names]
    p$layers[[layer.i]]$mapping <- layer.aes
    if(!is.data.frame(p$layers[[layer.i]]$data)){
      p$layers[[layer.i]]$data <- p$data
    }
  }
  # Test fill and color to see if they encode a quantitative
  # variable. This may be useful for several reasons: (1) it is
  # sometimes possible to plot several different colors in the same
  # trace (e.g. points), and that is faster for large numbers of
  # data points and colors; (2) factors on x or y axes should be
  # sent to plotly as characters, not as numeric data (which is
  # what ggplot_build gives us).
  misc <- list()
  for (a in c("fill", "colour", "x", "y", "size")) {
    for(data.type in c("continuous", "date", "datetime", "discrete")){
      fun.name <- sprintf("scale_%s_%s", a, data.type)
      misc.name <- paste0("is.", data.type)
      misc[[misc.name]][[a]] <- tryCatch({
        fun <- get(fun.name)
        suppressMessages({
          with.scale <- p + fun()
        })
        ggplot_build(with.scale)
        TRUE
      }, error=function(e){
        FALSE
      })
    }
  }
  
  ## scales are needed for legend ordering.
  misc$breaks <- list()
  for(sc in p$scales$scales){
    a.vec <- sc$aesthetics
    default.breaks <- inherits(sc$breaks, "waiver")
    if (length(a.vec) == 1 && (!default.breaks) ) {
      ## TODO: generalize for x/y scales too.
      br <- sc$breaks
      ranks <- seq_along(br)
      names(ranks) <- br
      misc$breaks[[a.vec]] <- ranks
    }
    ## store if this is a reverse scale so we can undo that later.
    if(is.character(sc$trans$name)){
      misc$trans[sc$aesthetics] <- sc$trans$name
    }
  }
  reverse.aes <- names(misc$trans)[misc$trans=="reverse"]
  
  # Extract data from built ggplots
  built <- ggplot_build2(p)
  # Get global ranges now because we need some of its info in layer2traces
  ranges.list <- list()
  for(xy in c("x", "y")){
    use.ranges <-
      misc$is.continuous[[xy]] ||
      misc$is.date[[xy]] ||
      misc$is.datetime[[xy]] 
    range.values <- if(use.ranges){
      range.name <- paste0(xy, ".range")
      sapply(built$panel$ranges, "[[", range.name)
    }else{
      ## for categorical variables on the axes, panel$ranges info is
      ## meaningless.
      name.name <- paste0(xy, ".name")
      sapply(built$data, function(df){
        if(name.name %in% names(df)){
          ## usually for discrete data there is a .name column.
          paste(df[[name.name]])
        }else{
          ## for heatmaps there may not be.
          df[[xy]]
        }
      })
    }
    ranges.list[[xy]] <- range(range.values)
  }
  
  # Get global size range because we need some of its info in layer2traces
  if ("size.name" %in% name.names) {
    sizerange <- sapply(built$prestats.data, `[[`, "size")
    ggsizemin <- min(unlist(sizerange))
    ggsizemax <- max(unlist(sizerange))
  }

  layer.legends <- list()
  for(i in seq_along(built$plot$layers)){
    # This is the layer from the original ggplot object.
    L <- p$layers[[i]]
    
    # for consistency in the layout between grid/wrap
    if (!inherits(p$facet, "wrap")) {
      built$panel$layout$AXIS_X <- with(built$panel$layout, ROW == max(ROW))
      built$panel$layout$AXIS_Y <- with(built$panel$layout, COL == 1)
    }
    
    # TODO: do we really need to merge?
    df <- merge(built$data[[i]], built$panel$layout, sort = FALSE)
    
    prestats <- built$prestats.data[[i]]
    # scale_reverse multiples x/y data by -1, so here we undo that so
    # that the actual data can be uploaded to plotly.
    replace.aes <- intersect(names(prestats), reverse.aes)
    for (a in replace.aes) {
      prestats[[a]] <- -1 * prestats[[a]]
    }
    # need layout in layers2traces() in order to place traces on correct axis
    L$prestats.data <- merge(prestats, built$panel$layout, sort = FALSE)
    
    # Add global range info.
    for(xy in names(ranges.list)){
      range.vec <- ranges.list[[xy]]
      names(range.vec) <- c("min", "max")
      for(range.name in names(range.vec)){
        glob.name <- paste0("glob", xy, range.name)
        L$prestats.data[[glob.name]] <- range.vec[[range.name]]
      }
    }
    
    # Add global size info if relevant
    if ("size.name" %in% name.names) {
      L$prestats.data$globsizemin <- ggsizemin
      L$prestats.data$globsizemax <- ggsizemax
    }
    
    # This extracts essential info for this geom/layer.
    traces <- layer2traces(L, df, misc)
    
    possible.legends <- markLegends[[type(L, "geom")]]
    actual.legends <- possible.legends[possible.legends %in% names(L$mapping)]
    layer.legends[[paste(i)]] <- actual.legends
    
    trace.list <- c(trace.list, traces)
  }
  
  # for barcharts, verify that all traces have the same barmode; we don't
  # support different barmodes on the same plot yet.
  barmodes <- do.call(c, lapply(trace.list, function (x) x$barmode))
  barmodes <- barmodes[!is.null(barmodes)]
  if (length(barmodes) > 0) {    
    layout$barmode <- barmodes[1]
    if (!all(barmodes == barmodes[1])) {
      warning(
        "You have multiple barcharts or histograms with different positions; ",
        "Plotly's layout barmode will be '", layout$barmode, "'."
      )
    }
    # for stacked bar charts, plotly cumulates bar heights, but ggplot doesn't
    if (layout$barmode == "stack") {
      # could speed up this function with environments or C/C++
      unStack <- function(vec) {
        n <- length(vec)
        if (n == 1) return(vec)
        seq.n <- seq_len(n)
        names(vec) <- seq.n
        vec <- sort(vec)
        for (k in seq(2, n)) {
          vec[k] <- vec[k] - sum(vec[seq(1, k-1)])
        }
        as.numeric(vec[as.character(seq.n)])
      }
      ys <- lapply(trace.list, "[[", "y")
      xs <- lapply(trace.list, "[[", "x")
      x.vals <- unique(unlist(xs))
      # if there are two or more y-values (for a particular x value),
      # then modify those y-values so they *add up* to the correct value(s)
      for (val in x.vals) {
        zs <- lapply(xs, function(x) which(x == val))
        ys.given.x <- Map(function(x, y) y[x], zs, ys)
        if (length(unlist(ys.given.x)) < 2) next
        st <- unStack(unlist(ys.given.x))
        lens <- sapply(ys.given.x, length)
        trace.seq <- seq_along(trace.list)
        ws <- split(st, rep(trace.seq, lens))
        for (tr in seq_along(ws)) {
          idx <- zs[[tr]]
          replacement <- ws[[tr]]
          if (length(idx) > 0  && length(replacement) > 0) 
            trace.list[[tr]]$y[idx] <- replacement
        }
      }
    }
  }
  
  # Bar Gap for histograms should be 0
  bargaps <- do.call(c, lapply(trace.list, function (x) x$bargap))
  if (length(bargaps) > 0) {
    if (any(bargaps == 0)) {
      layout$bargap <- 0
      if (!all(bargaps == 0)) {
        warning("You have multiple bar charts and histograms;\n
              Plotly's layout bargap will be 0 for all of them.")
      }
    } else {
      bargaps <- NULL  # Do not specify anything
    }
  }
  
  # obtain _calculated_ theme elements
  theme.pars <- getFromNamespace("plot_theme", "ggplot2")(p)
  for (i in names(theme.pars)) {
    theme.pars[[i]] <- ggplot2::calc_element(i, theme.pars)
  }
  # Background color.
  layout$plot_bgcolor <- toRGB(theme.pars$panel.background$fill)
  layout$paper_bgcolor <- toRGB(theme.pars$plot.background$fill)
  
  # generic function for translating element_text() to a plotly font object
  text2font <- function(x) {
    list(
      color = toRGB(x$colour),
      family = x$family,
      size = if (length(x$size)) unit2pixels(grid::unit(x$size, "points")) else 0
    )
  }
  # global font (all other font objects inherit from it)
  layout$font <- text2font(theme.pars$text)
  # Main plot title.
  layout$title <- faced(built$plot$labels$title, theme.pars$plot.title$face)
  layout$titlefont <- text2font(theme.pars$plot.title)
  # avoid sending redundant info
  if (identical(layout$font, layout$titlefont)) layout$titlefont <- NULL
  
  # remaining floating variables that should die eventually 
  layout$margin <- list()
  # TODO: apparently it is possible for [x/y] to be both? (fix it above)
  xDiscrete <- misc$is.discrete[["x"]] && !misc$is.continuous[["x"]]
  yDiscrete <- misc$is.discrete[["y"]] && !misc$is.continuous[["y"]]
  
  # Translate [x/y]axis 
  for (xy in c("x", "y")) {
    # find axis specific theme elements that inherit from their parent
    theme_el <- function(el) {
      theme.pars[[paste0(el, ".", xy)]] %||% theme.pars[[el]]
    }
    axisTicks <- theme_el("axis.ticks")
    axisText <- theme_el("axis.text")
    axisTitle <- theme_el("axis.title")
    axisLine <- theme_el("axis.line")
    panelGrid <- theme_el("panel.grid.major")
    # TODO: 
    # (1) We currently ignore minor grid lines. Would it makes sense to 
    # create another axis object just for the minor grid?
    # (2) translate panel.border to a rect shape
    # (3) Does it make sense to translate other theme elements?
    sc <- p$scales$get_scales(xy)
    # set some axis defaults (and override some of them later)
    # https://plot.ly/r/reference/#layout-xaxis
    axisObj <- list(
      title = if (!is.null(axisTitle)) sc$name %||% built$plot$labels[[xy]],
      titlefont = text2font(axisTitle),
      type = "linear",
      autorange = FALSE,
      tickmode = "array",
      range = built$panel$ranges[[1]][[paste0(xy, ".range")]],
      ticktext = built$panel$ranges[[1]][[paste0(xy, ".labels")]],
      tickvals = built$panel$ranges[[1]][[paste0(xy, ".major")]],
      ticks = if (is_blank(axisTicks)) "" else "outside",
      tickcolor = toRGB(axisTicks$colour),
      ticklen = unit2pixels(theme.pars$axis.ticks.length),
      # TODO: convert this size? Is this in points? If so, be careful to check length b4 using grid::unit()
      tickwidth = axisTicks$size,
      showticklabels = TRUE,
      tickfont = text2font(axisText),
      tickangle = -axisText$angle,
      showline = !is_blank(axisLine),
      linecolor = toRGB(axisLine$colour),
      # TODO: convert this size? Is this in points?
      linewidth = axisLine$size,
      showgrid = !is_blank(panelGrid),
      gridcolor = toRGB(panelGrid$colour),
      # TODO: convert this size? Is this in points?
      gridwidth = panelGrid$size,
      zeroline = FALSE  # ggplot2 never shows zero lines
    )
    # bold/italisixe title
    axisObj$title <- faced(axisObj$title, theme.pars$axis.text$face)
    
    disc <- if (xy == "x") xDiscrete else yDiscrete
    axisObj <- re_scale(axisObj, disc)
    
    # tack axis object onto the layout
    layout[[paste0(xy, "axis")]] <- axisObj
    
    # margins for add axis title/text
    side <- if (xy == "x") "b" else "l"
    # plotly.js has no sense of padding between axis tick and text so we
    # just support _bottom_ x-axis text & _left_ y-axis margins
    idx <- if (xy == "x") 3 else 4
    # 'size' refers to vertical size of a character
    # this is a rough estimation of the horiziontal size
    horizSize <- max(nchar(axisObj$ticktext)) / 2
    # account for the angle of the text
    radians <- abs(axisObj$tickangle * (pi/180))
    # tickangle of 0 means horizontal & sin(0) == 0
    trig <- if (xy == "x") sin else cos
    mult <- horizSize * trig(radians) + 1
    layout$margin[[side]] <- unit2pixels(axisTitle$margin[idx]) + 
      unit2pixels(axisText$margin[idx]) + 
      mult * axisObj$tickfont$size + axisObj$titlefont$size
  }
  
  if (has_facet(p)) {
    # each axis should _not_ have it's own title!
    layout$annotations <- c(
      layout$annotations, 
      # TODO: spacing should respect theme(axis.text)
      make.label(layout$xaxis$title, 0.5, -0.05, 
                 yanchor = "top"),
      make.label(layout$yaxis$title, -0.05, 0.5, 
                 xanchor = "bottom", textangle = -90)
    )
    layout <- strip_axis(layout)
    gglayout <- built$panel$layout
    npanels <- nrow(gglayout)
    # TODO: 
    # (1) hMargin should respect theme(axis.text.y)
    # (2) vMargin should respect theme(axis.text.x) + theme(strip.text) 
    hMargin <- if (isTRUE(p$facet$free$y)) c(.02, .02) else c(0, 0)
    vMargin <- if (isTRUE(p$facet$free$x)) c(.08, .08) else c(0, 0)
    # determine the appropriate domains/anchor for each panel
    doms <- get_domains(npanels, max(gglayout$ROW), c(hMargin, vMargin))
    gglayout$XAXIS <- with(gglayout, ifelse(AXIS_X, PANEL, COL))
    gglayout$YAXIS <- with(gglayout, ifelse(AXIS_Y, PANEL, ROW))
    for (i in seq_len(npanels)) {
      lay <- gglayout[i, ]
      # tack on domain anchors to the existing [x/y]axis
      # and create new objects if necessary
      x <- sub("1", "", lay$XAXIS)
      y <- sub("1", "", lay$YAXIS)
      xaxis <- list(
        domain = sort(as.numeric(doms[i, c("xstart", "xend")])),
        anchor = paste0("y", y),
        range = built$panel$ranges[[i]][["x.range"]],
        ticktext = built$panel$ranges[[i]][["x.labels"]],
        tickvals = built$panel$ranges[[i]][["x.major"]]
      )
      layout[[paste0("xaxis", x)]] <- modifyList(layout[["xaxis"]], xaxis)
      layout[[paste0("xaxis", x)]] <- re_scale(layout[[paste0("xaxis", x)]], xDiscrete)
      yaxis <- list(
        domain = sort(as.numeric(doms[i, c("ystart", "yend")])),
        anchor = paste0("x", x),
        range = built$panel$ranges[[i]][["y.range"]],
        ticktext = built$panel$ranges[[i]][["y.labels"]],
        tickvals = built$panel$ranges[[i]][["y.major"]]
      )
      layout[[paste0("yaxis", y)]] <- modifyList(layout[["yaxis"]], yaxis)
      layout[[paste0("yaxis", y)]] <- re_scale(layout[[paste0("yaxis", y)]], yDiscrete)
      # annotations for facet strip text
      # TODO: 
      # (1) use built$plot$facet$labeller for the actual strip text!
      # (2) use a rect shape for the strip background!
      if (inherits(p$facet, "grid")) {
        col_txt <- paste(lay[, as.character(built$plot$facet$cols)], collapse = ", ")
        row_txt <- paste(lay[, as.character(built$plot$facet$rows)], collapse = ", ")
          layout$annotations <- c(
            layout$annotations,
            make.label(col_txt, mean(xaxis$domain), max(yaxis$domain), 
                       xanchor = "center", yanchor = "bottom"),
            make.label(row_txt, max(xaxis$domain), mean(yaxis$domain), 
                       xanchor = "bottom", yanchor = "center")
          )
      } else {
        txt <- paste(lay[, as.character(built$plot$facet$facets)], collapse = ", ")
        layout$annotations <- c(
          layout$annotations,
          make.label(txt, mean(xaxis$domain), max(yaxis$domain), 
                     xanchor = "center", yanchor = "bottom")
        )
      }
    }
  }
  ### TODO: trace
  ## order traces according to the order of the scale
  ## plotly.js may provide a more elegant solution to this
  ## https://github.com/plotly/plotly.js/issues/189
  #nms <- unlist(lapply(traces, "[[", "name"))
  #trace.list <- trace.list[match(nms, axisObj$range)]
  ## remove traces that are outside the range of (discrete) scales
  #if (!is.null(sc$limits)) {
  #  trace.list <- trace.list[nms %in% sc$limits]
  #}
  
  # TODO: I don't think we need to worry about transformations?
  #if (identical(sc$trans$name, "reverse")) {
  #  axisObj$range <- sort(-axisObj$range, decreasing = TRUE)
  #}
  
  layout$legend <- list(
    bordercolor = "transparent", 
    x = 1.01, 
    y = 0.075 * 0.5* length(trace.list) + 0.45,
    xref="paper", yref="paper",
    xanchor = "left", yanchor = "top"
  )
  
  ## Legend hiding when guides(fill="none").
  legends.present <- unique(unlist(layer.legends))
  is.false <- function(x){
    is.logical(x) && length(x) == 1 && x == FALSE
  }
  is.none <- function(x){
    is.character(x) && length(x) == 1 && x == "none"
  }
  is.hidden <- function(x){
    is.false(x) || is.none(x)
  }
  layout$showlegend <- if(length(legends.present) == 0) FALSE else TRUE
  for(a in legends.present){
    if(is.hidden(p$gFuides[[a]])){
      layout$showlegend <- FALSE
    }
  }
  # Legend hiding from theme.
  if(theme.pars$legend.position=="none"){
    layout$showlegend <- FALSE
  }
  
  # Only show a legend title if there is at least 1 trace with
  # showlegend=TRUE.
  trace.showlegend <- sapply(trace.list, "[[", "showlegend")
  if (any(trace.showlegend) && layout$showlegend && length(p$data)) {
      # Retrieve legend title
      temp.title <- guide_names(p)
      legend.title <- if (length(unique(temp.title)) > 1){
        paste(temp.title, collapse = " / ")
      } else {
        unique(temp.title)
      }
    legend.title <- paste0("<b>", legend.title, "</b>")
    
    # Create legend title element as an annotation
    layout$annotations <- c(
      layout$annotations,
      list(
        text = faced(legend.title, theme.pars$legend.text$face),
        x = layout$legend$x * 1.0154,
        y = 0.075 * 0.5* length(trace.list) + 0.55,
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "top",
        textangle = 0
      )
    )
  }
  
  layout$legend$font <- text2font(theme.pars$legend.text)
  # avoid sending redundant info
  if (identical(layout$font, layout$legend$font)) layout$legend$font <- NULL
  
  mode.mat <- matrix(NA, 3, 3)
  rownames(mode.mat) <- colnames(mode.mat) <- c("markers", "lines", "none")
  mode.mat["markers", "lines"] <-
    mode.mat["lines", "markers"] <- "lines+markers"
  mode.mat["markers", "none"] <- mode.mat["none", "markers"] <- "markers"
  mode.mat["lines", "none"] <- mode.mat["none", "lines"] <- "lines"
  merged.traces <- list()
  not.merged <- trace.list
  while(length(not.merged)){
    tr <- not.merged[[1]]
    not.merged <- not.merged[-1]
    # Are there any traces that have not yet been merged, and can be
    # merged with tr?
    can.merge <- logical(length(not.merged))
    for(other.i in seq_along(not.merged)){
      other <- not.merged[[other.i]]
      criteria <- c()
      for(must.be.equal in c("x", "y", "xaxis", "yaxis")){
        other.attr <- other[[must.be.equal]]
        tr.attr <- tr[[must.be.equal]]
        criteria[[must.be.equal]] <- 
          isTRUE(all.equal(other.attr, tr.attr)) && 
          unique(other$type, tr$type) == "scatter"
      }
      if(all(criteria)){
        can.merge[[other.i]] <- TRUE
      }
    }
    to.merge <- not.merged[can.merge]
    not.merged <- not.merged[!can.merge]
    for(other in to.merge){
      new.mode <- tryCatch({
        mode.mat[tr$mode, other$mode]
      }, error=function(e){
        NA
      })
      if(is.character(new.mode) && !is.na(new.mode %||% NA)){
        tr$mode <- new.mode
      }
      attrs <- c("error_x", "error_y", "marker", "line")
      for(attr in attrs){
        if(!is.null(other[[attr]]) && is.null(tr[[attr]])){
          tr[[attr]] <- other[[attr]]
        }
      }
    }
    merged.traces[[length(merged.traces)+1]] <- tr
  }
  
  # -------------------------------
  # avoid redundant legends entries
  # -------------------------------
  # remove alpha from a color entry
  rm_alpha <- function(x) {
    if (length(x) == 0) return(x)
    pat <- "^rgba\\("
    if (!grepl(pat, x)) return(x)
    sub(",\\s*[0]?[.]?[0-9]+\\)$", ")", sub(pat, "rgb(", x))
  }
  # convenient for extracting name/value of legend entries (ignoring alpha)
  entries <- function(x, y) {
    z <- try(x[[y]], silent = TRUE)
    if (inherits(z, "try-error")) {
      paste0(x$name, "-")
    } else {
      paste0(x$name, "-", rm_alpha(z))
    }
  }
  fill_set <- unlist(lapply(merged.traces, entries, "fillcolor"))
  line_set <- unlist(lapply(merged.traces, entries, c("line", "color")))
  mark_set <- unlist(lapply(merged.traces, entries, c("marker", "color")))
  mode_set <- lapply(merged.traces, "[[", "mode")
  legend_intersect <- function(x, y) {
    i <- intersect(x, y)
    # restrict intersection to valid legend entries
    i[grepl("-rgb[a]?\\(", i)]
  }
  # if there is a mark & line legend, get rid of line
  t1 <- line_set %in% legend_intersect(mark_set, line_set)
  # that is, unless the mode is 'lines+markers'...
  t1 <- t1 & !(mode_set %in% "lines+markers")
  # if there is a mark & fill legend, get rid of fill
  t2 <- fill_set %in% legend_intersect(mark_set, fill_set)
  # if there is a line & fill legend, get rid of fill
  t3 <- fill_set %in% legend_intersect(line_set, fill_set)
  t <- t1 | t2 | t3
  for (m in seq_along(merged.traces)) 
    if (isTRUE(merged.traces[[m]]$showlegend && t[m]))
      merged.traces[[m]]$showlegend <- FALSE
  
  # Put the traces in correct order, according to any manually
  # specified scales. This seems to be repetitive with the trace$rank
  # attribute in layer2traces (which is useful for sorting traces that
  # get different legend entries but come from the same geom, as in
  # test-ggplot-legend.R), but in fact this is better since it could
  # be used for sorting traces that come from different geoms
  # (currently we don't have a test for this). TODO: write such a
  # test, delete the trace$rank code, and have it work here instead.
  
  #trace.order <- unlist(merged.traces)
  #ordered.traces <- if(length(trace.order)){
  #  trace.order.score <- seq_along(trace.order)
  #  names(trace.order.score) <- trace.order
  #  trace.name <- sapply(merged.traces, "[[", "name")
  #  trace.score <- trace.order.score[trace.name]
  #  merged.traces[order(trace.score)]
  #}else{
  #  merged.traces
  #}
  
  # If coord_flip is defined, then flip x/y in each trace, and in
  # each axis.
  flipped.traces <- merged.traces
  flipped.layout <- layout
  coord_cl <- sub("coord", "", tolower(class(built$plot$coordinates)))
  if ("flip" %in% coord_cl) {
    if (has_facet()) warning("coord_flip + facet conversion not supported by plotly")
    for(trace.i in seq_along(flipped.traces)){
      tr <- flipped.traces[[trace.i]]
      x <- tr[["x"]]
      y <- tr[["y"]]
      tr[["y"]] <- x
      tr[["x"]] <- y
      if (isTRUE(tr[["type"]] == "bar")) tr$orientation <- "h"
      flipped.traces[[trace.i]] <- tr
    }
    x <- layout[["xaxis"]]
    y <- layout[["yaxis"]]
    flipped.layout[["xaxis"]] <- y
    flipped.layout[["yaxis"]] <- x
  }
  l <- list(data = flipped.traces, layout = flipped.layout)
  # translate plot margin
  pm <- unit2pixels(theme.pars$plot.margin)
  title <- if (is.null(built$plot$labels$title)) 0 else unit2pixels(theme.pars$plot.title$size)
  l <- add_margin(l, pm[[1]] + title, pm[[2]], pm[[3]], pm[[4]])
  structure(add_boxed(rm_asis(l)), class = "plotly")
}

#-----------------------------------------------------------------------------
# ggplotly 'utility' functions
make.label <- function(text, x, y, xanchor = "auto", yanchor = "auto", 
                       textangle = 0) {
  list(list(
    text = text, 
    showarrow = FALSE, 
    x = x, 
    y = y, 
    ax = 0, 
    ay = 0, 
    xref = "paper", 
    yref = "paper", 
    xanchor = xanchor, 
    yanchor = yanchor, 
    textangle = textangle
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

#' Add plot margins
#' 
#' @param p a plotly (or ggplot2) object
#' @param t the top margin
#' @param r the right margin
#' @param b the bottom margin
#' @param l the left margin

add_margin <- function(p, t = 0, r = 0, b = 0, l = 0) {
  p <- plotly_build(p)
  p$layout$margin$t <- p$layout$margin$t %||% 0 + t
  p$layout$margin$r <- p$layout$margin$r %||% 0 + r
  p$layout$margin$b <- p$layout$margin$b %||% 0 + b
  p$layout$margin$l <- p$layout$margin$l %||% 0 + l
  # TODO: expose this to users??
  p
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


re_scale <- function(axisObj, discrete = TRUE) {
  if (discrete) {
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
