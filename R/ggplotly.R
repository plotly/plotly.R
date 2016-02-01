#' Create plotly graphs using ggplot2 syntax
#'
#' See up-to-date documentation and examples at
#' \url{https://plot.ly/ggplot2}
#'
#' @param p a ggplot object.
#' @param width	Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @seealso \link{signup}, \link{plot_ly}
#' @import httr jsonlite
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
  # Translate plot wide theme elements
  layout$plot_bgcolor <- toRGB(theme.pars$panel.background$fill)
  layout$paper_bgcolor <- toRGB(theme.pars$plot.background$fill)
  # Initiate the plot margin
  pm <- unit2pixels(theme.pars$plot.margin)
  layout$margin <- list(t = pm[[1]], r = pm[[2]], b = pm[[3]], l = pm[[4]])
  
  # global font (all other font objects inherit from it)
  layout$font <- text2font(theme.pars$text)
  # Main plot title.
  layout$title <- faced(built$plot$labels$title, theme.pars$plot.title$face)
  layout$titlefont <- text2font(theme.pars$plot.title)
  if (!is.null(layout[["title"]])) {
    layout$margin$t <- layout$margin$t + layout$titlefont$size
  }
  # before computing panel domains, figure out the margins _between_
  # panels (only relevant for free x/y scales and interior column strips)
  yAxisText <- theme.pars[["axis.text.y"]] %||% theme.pars[["axis.text"]]
  xAxisText <- theme.pars[["axis.text.x"]] %||% theme.pars[["axis.text"]]
  pts2npc <- function(x) {
    as.numeric(grid::convertUnit(grid::unit(x, "points"), "npc"))
  }
  yAxisSize <- pts2npc(yAxisText$size)
  xAxisSize <- pts2npc(xAxisText$size)
  stripText <- theme.pars[["strip.text.x"]] %||% theme.pars[["strip.text"]]
  stripSize <- pts2npc(stripText$size)
  # assume no margin between panels, and add margins, if appropriate
  margins <- rep(0, 4)
  gglayout <- built$panel$layout
  # space for interior yaxes
  if (isTRUE(p$facet$free$y) && max(gglayout$COL) > 1) {
    margins[1:2] <- yAxisSize
  }
  # space for interior column strips
  if (isTRUE(p$facet$free$y) && max(gglayout$COL) > 1) {
    margins[3:4] <- stripSize
  }
  # space for interior xaxes
  if (isTRUE(p$facet$free$x) && max(gglayout$ROW) > 1) {
    margins[3:4] <- margins[3:4] + xAxisSize
  }
  
  # determine the appropriate domains/anchor for each panel
  npanels <- nrow(gglayout)
  doms <- get_domains(npanels, max(gglayout$ROW), margins)
  gglayout$xaxis <- with(gglayout, ifelse(AXIS_X, PANEL, COL))
  gglayout$xaxis <- paste0("xaxis", sub("1", "", gglayout$xaxis))
  gglayout$yaxis <- with(gglayout, ifelse(AXIS_Y, PANEL, ROW))
  gglayout$yaxis <- paste0("yaxis", sub("1", "", gglayout$yaxis))
  
  for (i in seq_len(npanels)) {
    lay <- gglayout[i, ]
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
      stripText <- theme_el("strip.text")
      # TODO: 
      # (1) We currently ignore minor grid lines. Would it makes sense to 
      # create another axis object just for the minor grid?
      # (2) translate panel.border to a rect shape
      # (3) Does it make sense to translate other theme elements?
      
      axisName <- lay[, paste0(xy, "axis")]
      anchor <- sub("axis", "", lay[, paste0(setdiff(c("x", "y"), xy), "axis")])
      rng <- built$panel$ranges[[i]]
      
      sc <- p$scales$get_scales(xy)
      # set some axis defaults (and override some of them later)
      # https://plot.ly/r/reference/#layout-xaxis
      axisObj <- list(
        title = if (!is.null(axisTitle)) sc$name %||% built$plot$labels[[xy]],
        titlefont = text2font(axisTitle),
        type = "linear",
        autorange = FALSE,
        tickmode = "array",
        range = rng[[paste0(xy, ".range")]],
        ticktext = rng[[paste0(xy, ".labels")]],
        tickvals = rng[[paste0(xy, ".major")]],
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
        domain = sort(as.numeric(doms[i, paste0(xy, c("start", "end"))])),
        gridcolor = toRGB(panelGrid$colour),
        # TODO: convert this size? Is this in points?
        gridwidth = panelGrid$size,
        zeroline = FALSE,  # ggplot2 never shows zero lines
        anchor = anchor
      )
      # bold/italic axis title
      axisObj$title <- faced(axisObj$title, theme.pars$axis.text$face)
      
      # TODO: apparently it is possible for [x/y] to be both? (fix it above)
      disc <- misc$is.discrete[[xy]] && !misc$is.continuous[[xy]]
      axisObj <- re_scale(axisObj, disc)
      
      # tack axis object onto the layout
      layout[[axisName]] <- axisObj
      
      # account for axis title/text in plot margins
      side <- if (xy == "x") "b" else "l"
      way <- if (xy == "x") "v" else "h"
      # just support _bottom_ x-axis text & _left_ y-axis margins
      # (plotly.js has no sense of padding between ticks and ticktext)
      idx <- if (xy == "x") 3 else 4
      layout$margin[[side]] <- layout$margin[[side]] + 
        unit2pixels(axisTitle$margin[idx]) + 
        unit2pixels(axisText$margin[idx]) + 
        with(axisObj, bbox(ticktext, tickangle, tickfont$size))[[way]] +
        axisObj$titlefont$size
    } # axis loop
    
    xdom <- layout[[lay[, "xaxis"]]]$domain
    ydom <- layout[[lay[, "yaxis"]]]$domain
    
    # facet strips -> plotly annotations
    # TODO: 
    # (1) use a rect shape for the strip background!
    # (2) use built$plot$facet$labeller for the actual strip text!
    if (inherits(p$facet, "grid") && lay$COL == max(gglayout$COL)) {
      txt <- paste(
        lay[, as.character(built$plot$facet$rows)], 
        collapse = ", "
      )
      lab <- make_label(txt, mean(xdom), mean(ydom), 
                        xanchor = "center", yanchor = "bottom")
      layout$annotations <- c(layout$annotations, lab)
    }
    
    if (inherits(p$facet, "wrap") || 
        inherits(p$facet, "grid") && lay$ROW == 1) {
      vars <- ifelse(inherits(p$facet, "wrap"), "facets", "cols")
      txt <- paste(
        lay[, as.character(built$plot$facet[[vars]])], 
        collapse = ", "
      )
      lab <- make_label(txt, mean(xdom), max(ydom), 
                        xanchor = "center", yanchor = "bottom")
      layout$annotations <- c(layout$annotations, lab)
    }
  }   # panel loop

  if (has_facet(p)) {
    # space for the first row of strip text goes in plot margin 
    # (other rows are accounted for in the axes domains)
    xStripText <- theme.pars[["strip.text.x"]] %||% theme.pars[["strip.text"]]
    yStripText <- theme.pars[["strip.text.y"]] %||% theme.pars[["strip.text"]]
    layout$margin$t <- layout$margin$t + pts2npc(xStripText$size)
    layout$margin$r <- layout$margin$r + pts2npc(yStripText$size)
    
    # each axis should _not_ have it's own title!
    yAxes <- layout[grepl("^yaxis", names(layout))]
    xAxes <- layout[grepl("^xaxis", names(layout))]
    yTickText <- lapply(yAxes, "[[", "ticktext")
    xTickText <- lapply(xAxes, "[[", "ticktext")
    yTickTextMax <- yTickText[max(nchar(yTickText))]
    xTickTextMax <- xTickText[max(nchar(xTickText))]
    yPad <- with(layout$yaxis, bbox(yTickTextMax, tickangle, pts2npc(tickfont$size))$h)
    xPad <- with(layout$xaxis, bbox(xTickTextMax, tickangle, pts2npc(tickfont$size))$v)
    xAxisTitle <- theme.pars[["axis.title.x"]] %||% theme.pars[["axis.title"]]
    yAxisTitle <- theme.pars[["axis.title.y"]] %||% theme.pars[["axis.title"]]
    layout$annotations <- c(
      layout$annotations, 
      # TODO: what are the appropriate x/yanchors
      make_label(layout$xaxis$title, 0.5, -yPad, xAxisTitle, yanchor = "top"),
      make_label(layout$yaxis$title, -xPad, 0.5, yAxisTitle, xanchor = "top")
    )
    layout <- strip_axis(layout, "title")
  }

  ### TODO: trace ordering
  ## order traces according to the order of the scale
  ## plotly.js may provide a more elegant solution to this
  ## https://github.com/plotly/plotly.js/issues/189
  #nms <- unlist(lapply(traces, "[[", "name"))
  #trace.list <- trace.list[match(nms, axisObj$range)]
  ## remove traces that are outside the range of (discrete) scales
  #if (!is.null(sc$limits)) {
  #  trace.list <- trace.list[nms %in% sc$limits]
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
  structure(add_boxed(rm_asis(l)), class = "plotly")
}


#-----------------------------------------------------------------------------
# ggplotly 'utility' functions (and floating variables :/ )
#-----------------------------------------------------------------------------

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
  linetype = function(lty) {
    lty2dash[as.character(lty)]
  },
  colour = function(col) {
    toRGB(col)
  },
  size = mm2pixels,
  sizeref = identity,
  sizemode = identity,
  alpha = identity,
  shape = function(pch) {
    pch2symbol[as.character(pch)]
  },
  direction = identity
)

# NOTE: Do we also want to split on size?
# Legends based on sizes not implemented yet in Plotly
#  list(point=c("colour", "fill", "shape", "size"),
markLegends <- list(
  point = c("colour", "fill", "shape"),
  path = c("linetype", "size", "colour", "shape"),
  ## NOTE: typically "group" should not be present here, since
  ## that would mean creating a separate plotly legend for each
  ## group, even when they have the exact same visual
  ## characteristics and could be drawn using just 1 trace!
  polygon = c("colour", "fill", "linetype", "size"),
  bar = c("colour", "fill"),
  density = c("colour", "fill", "linetype"),
  boxplot = c("colour", "fill", "size"),
  errorbar = c("colour", "linetype"),
  errorbarh = c("colour", "linetype"),
  area = c("colour", "fill"),
  step = c("linetype", "size", "colour"),
  text = c("colour")
)

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
    size = if (length(x$size)) unit2pixels(grid::unit(x$size, "points")) else 0
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
