# calc. the epoch
now <- Sys.time()
the.epoch <- now - as.numeric(now)

default.marker.sizeref <- 1
marker.size.mult <- 10

marker.defaults <- list(alpha=1,
                        shape="16",
                        size=marker.size.mult,
                        sizeref=default.marker.sizeref,
                        sizemode="area",
                        colour="black")

line.defaults <- list(linetype="solid",
                      colour="black",
                      size=1,
                      direction="linear")

boxplot.defaults <- line.defaults
boxplot.defaults$colour <- "grey20"

ribbon.line.defaults <- line.defaults
ribbon.line.defaults$colour <- NA

polygon.line.defaults <- line.defaults
polygon.line.defaults$colour <- NA

# Convert R lty line type codes to plotly "dash" codes.
lty2dash <- c(numeric.lty, named.lty, coded.lty)

aesConverters <- list(
  linetype=function(lty) {
    lty2dash[as.character(lty)]
  },
  colour=function(col) {
    toRGB(col)
  },
  size=identity,
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
       errorbar=c("colour", "linetype"),
       errorbarh=c("colour", "linetype"),
       area=c("colour", "fill"),
       step=c("linetype", "size", "colour"),
       boxplot=c("x"),
       text=c("colour"))

markUnique <- as.character(unique(unlist(markLegends)))

#' Convert a ggplot to a list.
#' @import ggplot2
#' @param p ggplot2 plot.
#' @return list representing a ggplot.
#' @export
gg2list <- function(p){
  if(length(p$layers) == 0) {
    stop("No layers in plot")
  }
  # Always use identity size scale so that plot.ly gets the real
  # units for the size variables.
  original.p <- p
  p <- tryCatch({
    # this will be an error for discrete variables.
    suppressMessages({
      ggplot_build(p+scale_size_continuous())
      p+scale_size_identity()
    })
  },error=function(e){
    p
  })
  layout <- list()
  trace.list <- list()
  
  # Before building the ggplot, we would like to add aes(name) to
  # figure out what the object group is later. This also copies any
  # needed global aes/data values to each layer, so we do not have to
  # worry about combining global and layer-specific aes/data later.
  for(layer.i in seq_along(p$layers)) {
    layer.aes <- p$layers[[layer.i]]$mapping
    to.copy <- names(p$mapping)[!names(p$mapping) %in% names(layer.aes)]
    layer.aes[to.copy] <- p$mapping[to.copy]
    mark.names <- markUnique[markUnique %in% names(layer.aes)]
    name.names <- sprintf("%s.name", mark.names)
    layer.aes[name.names] <- layer.aes[mark.names]
    p$layers[[layer.i]]$mapping <- layer.aes
    if(!is.data.frame(p$layers[[layer.i]]$data)){
      p$layers[[layer.i]]$data <- p$data
    }
  }
  
  # Extract data from built ggplots
  built <- ggplot_build2(p)
  # Get global x-range now because we need some of its info in layer2traces
  ggranges <- built$panel$ranges
  # Extract x.range
  xrange <- sapply(ggranges, `[[`, "x.range", simplify=FALSE, USE.NAMES=FALSE)
  ggxmin <- min(sapply(xrange, min))
  ggxmax <- max(sapply(xrange, max))
  # Extract y.range
  yrange <- sapply(ggranges, `[[`, "y.range", simplify=FALSE, USE.NAMES=FALSE)
  ggymin <- min(sapply(yrange, min))
  ggymax <- max(sapply(yrange, max))
  
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
    
    # for each layer, there is a correpsonding data.frame which
    # evaluates the aesthetic mapping.
    df <- built$data[[i]]
    
    # Test fill and color to see if they encode a quantitative
    # variable. This may be useful for several reasons: (1) it is
    # sometimes possible to plot several different colors in the same
    # trace (e.g. points), and that is faster for large numbers of
    # data points and colors; (2) factors on x or y axes should be
    # sent to plotly as characters, not as numeric data (which is
    # what ggplot_build gives us).
    misc <- list()
    for(a in c("fill", "colour", "x", "y", "size")){
      for(data.type in c("continuous", "date", "datetime", "discrete")){
        fun.name <- sprintf("scale_%s_%s", a, data.type)
        misc.name <- paste0("is.", data.type)
        misc[[misc.name]][[a]] <- tryCatch({
          fun <- get(fun.name)
          suppressMessages({
            with.scale <- original.p + fun()
          })
          ggplot_build(with.scale)
          TRUE
        }, error=function(e){
          FALSE
        })
      }
    }
    
    # scales are needed for legend ordering.
    misc$breaks <- list()
    for(sc in p$scales$scales){
      a.vec <- sc$aesthetics
      default.breaks <- inherits(sc$breaks, "waiver")
      if (length(a.vec) == 1 && (!default.breaks) ) {
        # TODO: generalize for x/y scales too.
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
    
    # get gglayout now because we need some of its info in layer2traces
    gglayout <- built$panel$layout
    # invert rows so that plotly and ggplot2 show panels in the same order
    gglayout$plotly.row <- max(gglayout$ROW) - gglayout$ROW + 1
    # ugh, ggplot counts panel right-to-left & top-to-bottom
    # plotly count them right-to-left & *bottom-to-top*
    gglayout$plotly.panel <- with(gglayout, order(plotly.row, COL))
    
    # Add ROW and COL to df: needed to link axes to traces; keep df's
    # original ordering while merging.
    df$order <- seq_len(nrow(df))
    df <- merge(df, gglayout[, c("PANEL", "plotly.row", "COL")])
    df <- df[order(df$order),]
    df$order <- NULL
    
    prestats <- built$prestats.data[[i]]
    # scale_reverse multiples x/y data by -1, so here we undo that so
    # that the actual data can be uploaded to plotly.
    replace.aes <- intersect(names(prestats), reverse.aes)
    for (a in replace.aes) {
      prestats[[a]] <- -1 * prestats[[a]]
    }
    misc$prestats.data <-
      merge(prestats,
            gglayout[, c("PANEL", "plotly.row", "COL")])
    
    # Add global x-range info
    misc$prestats.data$globxmin <- ggxmin
    misc$prestats.data$globxmax <- ggxmax
    # Add global y-range info
    misc$prestats.data$globymin <- ggymin
    misc$prestats.data$globymax <- ggymax
    
    # Add global size info if relevant
    if ("size.name" %in% name.names) {
      misc$prestats.data$globsizemin <- ggsizemin
      misc$prestats.data$globsizemax <- ggsizemax
    }
    
    # This extracts essential info for this geom/layer.
    traces <- layer2traces(L, df, misc)
    
    possible.legends <- markLegends[[L$geom$objname]]
    actual.legends <- possible.legends[possible.legends %in% names(L$mapping)]
    layer.legends[[paste(i)]] <- actual.legends
    
    # Do we really need to coord_transform?
    # g$data <- ggplot2:::coord_transform(built$plot$coord, g$data,
    #                                     built$panel$ranges[[1]])
    trace.list <- c(trace.list, traces)
  }
  
  # for barcharts, verify that all traces have the same barmode; we don't
  # support different barmodes on the same plot yet.
  barmodes <- do.call(c, lapply(trace.list, function (x) x$barmode))
  barmodes <- barmodes[!is.null(barmodes)]
  if (length(barmodes) > 0) {    
    layout$barmode <- barmodes[1]
    if (!all(barmodes == barmodes[1]))
      warning(paste0("You have multiple barcharts or histograms with different positions; ",
                     "Plotly's layout barmode will be '", layout$barmode, "'."))
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
      # if there is more than one y-value (for a particular x value)
      # then 
      # 
      for (val in x.vals) {
        zs <- lapply(xs, function(x) which(x == val))
        ys.given.x <- Map(function(x, y) y[x], zs, ys)
        if (length(unlist(ys.given.x)) < 2) next
        st <- unStack(unlist(ys.given.x))
        lens <- sapply(ys.given.x, length)
        trace.seq <- seq_along(zs)
        ws <- split(st, rep(trace.seq, lens))
        for (tr in trace.seq) {
          idx <- zs[[tr]]
          if (length(idx)) trace.list[[tr]]$y[idx] <- ws[[tr]][idx]
        }
      }
    }
  }
    
#     lens <- sapply(ys, length)
#      && length(trace.list) > 1 && any(lens > 1)) {
#       xs <- unlist(xs)
#       trace.seq <- seq_along(trace.list)
#       idx <- rep(trace.seq, lens)
#       
#       
#       browser()
#       diffs <- tapply(unlist(ys), INDEX = xs, unStack)
#       for (k in trace.seq) trace.list[[k]]$y <- as.numeric(sapply(diffs, "[", k))
  
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
  
  # Export axis specification as a combination of breaks and labels, on
  # the relevant axis scale (i.e. so that it can be passed into d3 on the
  # x axis scale instead of on the grid 0-1 scale). This allows
  # transformations to be used out of the box, with no additional d3
  # coding.
  theme.pars <- ggplot2:::plot_theme(p)
  
  # Flip labels if coords are flipped - transform does not take care
  # of this. Do this BEFORE checking if it is blank or not, so that
  # individual axes can be hidden appropriately, e.g. #1.
  # ranges <- built$panel$ranges[[1]]
  # if("flip"%in%attr(built$plot$coordinates, "class")){
  #   temp <- built$plot$labels$x
  #   built$plot$labels$x <- built$plot$labels$y
  #   built$plot$labels$y <- temp
  # }
  e <- function(el.name){
    ggplot2::calc_element(el.name, p$theme)
  }
  is.blank <- function(el.name, null.is.blank=FALSE) {
    # NULL shows ticks and hides borders
    cls <- attr(e(el.name),"class")
    "element_blank" %in% cls || null.is.blank && is.null(cls)
  }
  trace.order.list <- list()
  trace.name.map <- c()
  for(xy in c("x","y")){
    ax.list <- list()
    coord.lim <- p$coord$limits[[xy]]
    if(is.numeric(coord.lim)){
      ## TODO: maybe test for more exotic coord specification types
      ## involving NA, Inf, etc?
      ax.list$range <- coord.lim
    }
    s <- function(tmp)sprintf(tmp, xy)
    ax.list$tickcolor <- toRGB(theme.pars$axis.ticks$colour)
    
    # When gridlines are dotted or dashed:
    grid <- theme.pars$panel.grid
    grid.major <- theme.pars$panel.grid.major
    if ((!is.null(grid$linetype) || !is.null(grid.major$linetype)) && 
        c(grid$linetype, grid.major$linetype) %in% c(2, 3, "dashed", "dotted")) {
      ax.list$gridcolor <- ifelse(is.null(grid.major$colour),
                                  toRGB(grid$colour, 0.1),
                                  toRGB(grid.major$colour, 0.1))
    } else {
      ax.list$gridcolor <- toRGB(grid.major$colour)
    }
    
    ax.list$showgrid <- !is.blank(s("panel.grid.major.%s"))
    # These numeric length variables are not easily convertible.
    #ax.list$gridwidth <- as.numeric(theme.pars$panel.grid.major$size)
    #ax.list$ticklen <- as.numeric(theme.pars$axis.ticks.length)
    
    theme2font <- function(text){
      if(!is.null(text)){
        list(family=text$family,
             size=text$size,
             color=toRGB(text$colour))
      }
    }
    # Ticks.
    if (is.blank("axis.ticks")) {
      ax.list$ticks <- ""
    } else if (is.blank(s("axis.ticks.%s"))) {
      ax.list$ticks <- ""
    } else {
      ax.list$ticks <- "outside"  # by default ggplot2 plots have ticks
    } 
    ax.list$tickwidth <- theme.pars$axis.ticks$size
    tick.text.name <- s("axis.text.%s")
    ax.list$showticklabels <- !is.blank(tick.text.name)
    tick.text <- e(tick.text.name)
    if (is.numeric(tick.text$angle)) {
      ax.list$tickangle <- -tick.text$angle
    }
    ax.list$tickfont <- theme2font(tick.text)

    ## determine axis type first, since this information is used later
    ## (trace.order.list is only used for type=category).
    title.text <- e(s("axis.title.%s"))
    ax.list$titlefont <- theme2font(title.text)
    ax.list$type <- if (misc$is.continuous[[xy]]){
      "linear"
    } else if (misc$is.discrete[[xy]]){
      "category"
    } else if (misc$is.date[[xy]] || misc$is.datetime[[xy]]){
      "date"
    } else {
      stop("unrecognized data type for ", xy, " axis")
    }
    
    # Translate axes labels.
    scale.i <- which(p$scales$find(xy))
    ax.list$title <- if(length(scale.i)){
      sc <- p$scales$scales[[scale.i]]
      if(ax.list$type == "category"){
        trace.order.list[[xy]] <- sc$limits
        if(is.character(sc$breaks)){
          if(is.character(sc$labels)){
            trace.name.map[sc$breaks] <- sc$labels
          }
          ##TODO: if(is.function(sc$labels)){
        }
      }
      if (is.null(sc$breaks)) {
        ax.list$showticklabels <- FALSE
        ax.list$showgrid <- FALSE
        ax.list$ticks <- ""
      }
      if (is.numeric(sc$breaks)) {
        dticks <- diff(sc$breaks)
        dt <- dticks[1]
        if(all(dticks == dt)){
          ax.list$dtick <- dt
          ax.list$autotick <- FALSE
        }
      }
      ax.list$range <- if(!is.null(sc$limits)){
        sc$limits
      }else{
        if(misc$is.continuous[[xy]]){
          ggranges[[1]][[s("%s.range")]] #TODO: facets!
        }else{ # for a discrete scale, range should be NULL.
          NULL
        }
      }
      if(is.character(sc$trans$name) && sc$trans$name == "reverse"){
        ax.list$range <- sort(-ax.list$range, decreasing = TRUE)
      }
      if(!is.null(sc$name)){
        sc$name
      }else{
        p$labels[[xy]]
      }
    }else{
      p$labels[[xy]]
    }

    ax.list$zeroline <- FALSE  # ggplot2 plots do not show zero lines
    # Lines drawn around the plot border.
    ax.list$showline <- !is.blank("panel.border", TRUE)
    ax.list$linecolor <- toRGB(theme.pars$panel.border$colour)
    ax.list$linewidth <- theme.pars$panel.border$size
    # Some other params that we used in animint but we don't yet
    # translate to plotly:
    !is.blank(s("axis.line.%s"))
    layout[[s("%saxis")]] <- ax.list
  }
  # copy [x/y]axis to [x/y]axisN and set domain, range, etc. for each
  xaxis.title <- layout$xaxis$title
  yaxis.title <- layout$yaxis$title
  inner.margin <- 0.01 # between facets
  outer.margin <- 0.05 # to put titles outside of the plots
  orig.xaxis <- layout$xaxis
  orig.yaxis <- layout$yaxis
  if (nrow(gglayout) > 1) {
    row.size <- 1. / max(gglayout$ROW)
    col.size <- 1. / max(gglayout$COL)
    npanels <- nrow(gglayout)
    for (i in seq_len(npanels)) {
      row <- gglayout[i, "plotly.row"]
      col <- gglayout[i, "COL"]
      panel <- gglayout[i, "plotly.panel"]
      x <- col * col.size
      xmin <- x - col.size
      xmax <- x - inner.margin
      y <- row * row.size
      ymin <- y - row.size
      ymax <- y - inner.margin
      # assume grid layout by default where axes are restrict to the exterior
      xaxis.name <- if (col == 1) "xaxis" else paste0("xaxis", col)
      yaxis.name <- if (row == 1) "yaxis" else paste0("yaxis", row)
      # anchor needs to be incremented if the corresponding axis is "free"
      xanchor <- "y"
      yanchor <- "x"
      if ("wrap" %in% class(p$facet)) {
        # in wrap layout, axes can be drawn on interior (if scales are free)
        # make room for facet strip label
        ymax <- ymax - 0.04
        # make room for yaxis labels (this should be a function of label size)
        if (col == 1) {
          xmax <- xmax - 0.02
        } else {
          xmin <- xmin + 0.02
        }
        # make room for xaxis labels
        if (row == 1) {
          ymax <- ymax - 0.02
        } else {
          ymin <- ymin + 0.02
        }
        if (p$facet$free$y && panel > 1) {
          # draw a y-axis on each panel
          yaxis.name <- paste0("yaxis", panel)
          trace.list[[i]]$yaxis <- paste0("y", panel)
          yanchor <- if (p$facet$free$x) paste0("x", panel) else paste0("x",col)
        } 
        if (p$facet$free$x && panel > 1) {
          # draw an x-axis on each panel
          xaxis.name <- paste0("xaxis", panel)
          trace.list[[i]]$xaxis <- paste0("x", panel)
          xanchor <- if (p$facet$free$y) paste0("y", panel) else paste0("y",row)
        }
      } 
      layout[[xaxis.name]] <- orig.xaxis
      layout[[xaxis.name]]$domain <- c(xmin, xmax)
      layout[[xaxis.name]]$anchor <- xanchor
      layout[[xaxis.name]]$title <- NULL
      layout[[yaxis.name]] <- orig.yaxis
      layout[[yaxis.name]]$domain <- c(ymin, ymax)
      layout[[yaxis.name]]$anchor <- yanchor
      layout[[yaxis.name]]$title <- NULL
      if (is.null(layout[[xaxis.name]]$anchor)) 
        layout[[xaxis.name]]$anchor <- "y"
      if (is.null(layout[[yaxis.name]]$anchor)) 
        layout[[yaxis.name]]$anchor <- "x"
      # range only makes sense for numeric data
      if (orig.xaxis$type == "linear") {
        layout[[xaxis.name]]$range <- built$panel$ranges[[i]]$x.range
        layout[[xaxis.name]]$autorange <- FALSE
      }
      if (orig.yaxis$type == "linear") {
        layout[[yaxis.name]]$range <- built$panel$ranges[[i]]$y.range
        layout[[yaxis.name]]$autorange <- FALSE
      }
    }
    # add panel titles as annotations
    annotations <- list()
    nann <- 1
    make.label <- function(text, x, y, xanchor="auto", yanchor="auto", textangle=0)
      list(text=text, showarrow=FALSE, x=x, y=y, ax=0, ay=0, 
           xref="paper", yref="paper", xanchor=xanchor, yanchor=yanchor, 
           textangle=textangle)
    if ("grid" %in% class(p$facet)) {
      frows <- names(p$facet$rows)
      nann <- 1
      
      for (i in seq_len(max(gglayout$ROW))) {
        text <- paste(lapply(gglayout[gglayout$ROW == i, frows, drop=FALSE][1,],
                             as.character),
                      collapse=", ")
        if (text != "") {  # to not create extra annotations
          increase_margin_r <- TRUE
          annotations[[nann]] <- make.label(text,
                                            1 + outer.margin - 0.04,
                                            row.size * (max(gglayout$ROW)-i+0.5),
                                            xanchor="center",
                                            textangle=90)
          nann <- nann + 1
        }
      }
      fcols <- names(p$facet$cols)
      for (i in seq_len(max(gglayout$COL))) {
        text <- paste(lapply(gglayout[gglayout$COL == i, fcols, drop=FALSE][1,],
                             as.character),
                      collapse=", ")
        if (text!="") {
          annotations[[nann]] <- make.label(text,
                                            col.size * (i-0.5) - inner.margin/2,
                                            1 + outer.margin,
                                            xanchor="center")
          nann <- nann + 1
        }
      }
      
      # add empty traces everywhere so that the background shows even if there
      # is no data for a facet
      for (r in seq_len(max(gglayout$ROW)))
        for (c in seq_len(max(gglayout$COL)))
          trace.list <- c(trace.list, list(list(xaxis=paste0("x", c), yaxis=paste0("y", r), showlegend=FALSE)))
    } else if ("wrap" %in% class(p$facet)) {
      facets <- names(p$facet$facets)
      for (i in seq_len(max(as.numeric(gglayout$PANEL)))) {
        ix <- gglayout$PANEL == i
        row <- gglayout$ROW[ix]
        col <- gglayout$COL[ix]
        text <- paste(lapply(gglayout[ix, facets, drop=FALSE][1,],
                             as.character),
                      collapse=", ")
        annotations[[nann]] <- make.label(text, 
                                          col.size * (col-0.5) - inner.margin/2,
                                          row.size * (max(gglayout$ROW) - row + 0.985),
                                          xanchor="center",
                                          yanchor="top")
        nann <- nann + 1
      }
    }
      # axes titles
      annotations[[nann]] <- make.label(xaxis.title, 
                                        0.5, 
                                        -outer.margin,
                                        yanchor="top")
      nann <- nann + 1
      annotations[[nann]] <- make.label(yaxis.title, 
                                        -outer.margin, 
                                        0.5,
                                        textangle=-90)
    layout$annotations <- annotations
  }
  
  # Remove legend if theme has no legend position
  layout$showlegend <- !(theme.pars$legend.position=="none")
  
  # Main plot title.
  layout$title <- built$plot$labels$title
  
  # Background color.
  layout$plot_bgcolor <- toRGB(theme.pars$panel.background$fill)
  layout$paper_bgcolor <- toRGB(theme.pars$plot.background$fill)
  
  # Legend.
  layout$margin$r <- 10
  if (exists("increase_margin_r")) {
    layout$margin$r <- 60
  }
  layout$legend <- list(bordercolor="transparent", 
                        x=1.05, y=1/2,
                        xanchor="center", yanchor="top")
  # Workaround for removing unnecessary legends.
  # [markUnique != "x"] is for boxplot's particular case.
  if (any(names(layer.aes) %in% markUnique[markUnique != "x"]) == FALSE)
    layout$showlegend <- FALSE

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
  for(a in legends.present){
    if(is.hidden(p$guides[[a]])){
      layout$showlegend <- FALSE
    }
  }
  
  # Only show a legend title if there is at least 1 trace with
  # showlegend=TRUE.
  trace.showlegend <- sapply(trace.list, "[[", "showlegend")
  if (any(trace.showlegend) && layout$showlegend && length(p$data)) {
    # Retrieve legend title
    legend.elements <- unlist(sapply(traces, "[[", "name"))
    legend.title <- ""
    for (i in 1:ncol(p$data)) {
      if (all(legend.elements %in% unique(p$data[, i])))
        legend.title <- colnames(p$data)[i]
    }
    legend.title <- paste0("<b>", legend.title, "</b>")
    
    # Create legend title element as an annotation
    if (exists("annotations")) {
      nann <- nann + 1
    } else {
      annotations <- list()
      nann <- 1
    }
    annotations[[nann]] <- list(text=legend.title,
                                x=layout$legend$x,
                                y=layout$legend$y * 1.04,
                                showarrow=FALSE,
                                xref="paper", yref="paper",
                                xanchor="center",
                                textangle=0)
    layout$annotations <- annotations
  }
  
  # Family font for text
  if (!is.null(theme.pars$text$family)) {
    layout$titlefont$family   <- theme.pars$text$family
    layout$legend$font$family <- theme.pars$text$family
  }
  
  # Family font for title
  if (!is.null(theme.pars$plot.title$family)) {
    layout$titlefont$family <- theme.pars$plot.title$family
  }
  
  # Family font for legend
  if (!is.null(theme.pars$legend.text$family)) {
    layout$legend$font$family <- theme.pars$legend.text$family
  }
  
  # Bold, italic and bold.italic face for text
  text_face <- theme.pars$text$face
  if (!is.null(text_face)) {
    if (text_face=="bold") {
      layout$title <- paste0("<b>", layout$title, "</b>")
      layout$yaxis$title <- paste0("<b>", layout$yaxis$title, "</b>")
      layout$xaxis$title <- paste0("<b>", layout$xaxis$title, "</b>")
    } else if (text_face=="italic") {
      layout$title <- paste0("<i>", layout$title, "</i>")
      layout$yaxis$title <- paste0("<i>", layout$yaxis$title, "</i>")
      layout$xaxis$title <- paste0("<i>", layout$xaxis$title, "</i>")
    } else if (text_face=="bold.italic") {
      layout$title <- paste0("<b><i>", layout$title, "</i></b>")
      layout$yaxis$title <- paste0("<b><i>", layout$yaxis$title, "</i></b>")
      layout$xaxis$title <- paste0("<b><i>", layout$xaxis$title, "</i></b>")
    }
  }
  
  # Bold, italic and bold.italic face for title
  title_face <- theme.pars$plot.title$face
  if (!is.null(title_face)) {
    if (title_face=="bold") {
      layout$title <- paste0("<b>", layout$title, "</b>")
    } else if (title_face=="italic") {
      layout$title <- paste0("<i>", layout$title, "</i>")
    } else if (title_face=="bold.italic") {
      layout$title <- paste0("<b><i>", layout$title, "</i></b>")
    }
  }
  
  # Bold, italic, and bold.italic face for axis title
  title_face <- list(theme.pars$axis.title.y$face,
                     theme.pars$axis.title.x$face)
  sub_elem <- c("yaxis", "xaxis")
  
  for (i in seq_along(title_face)) {
    if (!is.null(title_face[[i]])) {
      if (title_face[[i]]=="bold") {
        layout[[sub_elem[i]]]["title"] <- paste0("<b>",
                                                 layout[[sub_elem[i]]]["title"],
                                                 "</b>")
      } else if (title_face[[i]]=="italic") {
        layout[[sub_elem[i]]]["title"] <- paste0("<i>",
                                                 layout[[sub_elem[i]]]["title"],
                                                 "</i>")
      } else if (title_face[[i]]=="bold.italic") {
        layout[[sub_elem[i]]]["title"] <- paste0("<b><i>",
                                                 layout[[sub_elem[i]]]["title"],
                                                 "</b></i>")
      }
    }
  }
  
  # If background elements are NULL, and background rect (rectangle) is defined:
  rect_fill <- theme.pars$rect$fill
  if (!is.null(rect_fill)) {
    if (is.null(layout$plot_bgcolor))
      layout$plot_bgcolor <- toRGB(s(rect_fill))
    if (is.null(layout$paper_bgcolor))
      layout$paper_bgcolor <- toRGB(s(rect_fill))
    if (is.null(layout$legend$bgcolor))
      layout$legend$bgcolor <- toRGB(s(rect_fill))
  }
  
  if (length(trace.list) == 0) {
    stop("No exportable traces")
  }
  
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
    can.merge <- rep(FALSE, l=length(not.merged))
    for(other.i in seq_along(not.merged)){
      other <- not.merged[[other.i]]
      criteria <- c()
      for(must.be.equal in c("x", "y", "xaxis", "yaxis")){
        other.attr <- other[[must.be.equal]]
        tr.attr <- tr[[must.be.equal]]
        criteria[[must.be.equal]] <- isTRUE(all.equal(other.attr, tr.attr))
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
      if(is.character(new.mode) && !is.na(new.mode)){
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
    if (inherits(e, "try-error")) {
      paste0(x$name, "-")
    } else {
      paste0(x$name, "-", rm_alpha(z))
    }
  }
  fill_set <- unlist(lapply(merged.traces, entries, "fillcolor"))
  line_set <- unlist(lapply(merged.traces, entries, c("line", "color")))
  mark_set <- unlist(lapply(merged.traces, entries, c("marker", "color")))
  legend_intersect <- function(x, y) {
    i <- intersect(x, y)
    # restrict intersection to valid legend entries
    i[grepl("-rgb[a]?\\(", i)]
  }
  # if there is a mark & line legend, get rid of line
  t1 <- line_set %in% legend_intersect(mark_set, line_set)
  # that is, unless the mode is 'lines+markers'...
  t1 <- t1 & !(unlist(lapply(merged.traces, "[[", "mode")) %in% "lines+markers")
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
  trace.order <- unlist(trace.order.list)
  ordered.traces <- if(length(trace.order)){
    trace.order.score <- seq_along(trace.order)
    names(trace.order.score) <- trace.order
    trace.name <- sapply(merged.traces, "[[", "name")
    trace.score <- trace.order.score[trace.name]
    merged.traces[order(trace.score)]
  }else{
    merged.traces
  }
  
  # Translate scale(labels) to trace name.
  named.traces <- ordered.traces
  for(trace.i in seq_along(named.traces)){
    tr.name <- named.traces[[trace.i]][["name"]]
    new.name <- trace.name.map[[tr.name]]
    if(!is.null(new.name)){
      named.traces[[trace.i]][["name"]] <- new.name
    }
  }
  
  # If coord_flip is defined, then flip x/y in each trace, and in
  # each axis.
  flipped.traces <- named.traces
  flipped.layout <- layout
  if("flip" %in% attr(built$plot$coordinates, "class")){
    if(!inherits(p$facet, "null")){
      stop("coord_flip + facet conversion not supported")
    }
    for(trace.i in seq_along(flipped.traces)){
      tr <- flipped.traces[[trace.i]]
      x <- tr[["x"]]
      y <- tr[["y"]]
      tr[["y"]] <- x
      tr[["x"]] <- y
      flipped.traces[[trace.i]] <- tr
    }
    x <- layout[["xaxis"]]
    y <- layout[["yaxis"]]
    flipped.layout[["xaxis"]] <- y
    flipped.layout[["yaxis"]] <- x
  }
  
  flipped.traces$kwargs <- list(layout=flipped.layout)
  
  flipped.traces
}
