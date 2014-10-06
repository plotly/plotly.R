## calc. the epoch
now <- Sys.time()
the.epoch <- now - as.numeric(now)

##' Drawing ggplot2 geoms with a group aesthetic is most efficient in
##' plotly when we convert groups of things that look the same to
##' vectors with NA.
##' @param g list of geom info with g$data$group.
##' @param geom change g$geom to this.
##' @export
##' @return list of geom info.
##' @author Toby Dylan Hocking
group2NA <- function(g, geom){
  poly.list <- split(g$data, g$data$group)
  is.group <- names(g$data) == "group"
  poly.na.df <- data.frame()
  for(i in seq_along(poly.list)){
    no.group <- poly.list[[i]][,!is.group,drop=FALSE]
    na.row <- no.group[1,]
    na.row[,c("x", "y")] <- NA
    poly.na.df <- rbind(poly.na.df, no.group, na.row)
  }
  g$data <- poly.na.df
  g$geom <- geom
  g
}

#' Convert R pch point codes to plotly "symbol" codes.
pch2symbol <- c("0"="square",
                "1"="circle",
                "2"="triangle-up",
                "3"="cross",
                "4"="x",
                "5"="diamond",
                "6"="triangle-down",
                "15"="square",
                "16"="circle",
                "17"="triangle-up",
                "18"="diamond",
                "19"="circle",
                "20"="circle",
                "22"="square",
                "23"="diamond",
                "24"="triangle-up",
                "25"="triangle-down",
                "o"="circle",
                "O"="circle",
                "+"="cross")

#' Convert ggplot2 aes to plotly "marker" codes.
aes2marker <- c(alpha="opacity",
                colour="color",
                size="size",
                sizeref="sizeref",
                sizemode="sizemode",
                shape="symbol")

default.marker.sizeref <- 1
marker.size.mult <- 10

marker.defaults <- list(alpha=1,
                        shape="o",
                        size=marker.size.mult,
                        sizeref=default.marker.sizeref,
                        sizemode="area",
                        colour="black")
line.defaults <-
  list(linetype="solid",
       colour="black",
       size=2,
       direction="linear")

numeric.lty <-
  c("1"="solid",
    "2"="dash",
    "3"="dot",
    "4"="dashdot",
    "5"="longdash",
    "6"="longdashdot")

named.lty <-
  c("solid"="solid",
    "blank"="none",
    "dashed"="dash",
    "dotted"="dotted",
    "dotdash"="dashdot",
    "longdash"="longdash",
    "twodash"="dash")

## TODO: does plotly support this??
coded.lty <-
  c("22"="dash",
    "42"="dot",
    "44"="dashdot",
    "13"="longdash",
    "1343"="longdashdot",
    "73"="dash",
    "2262"="dotdash",
    "12223242"="dotdash",
    "F282"="dash",
    "F4448444"="dash",
    "224282F2"="dash",
    "F1"="dash")

#' Convert R lty line type codes to plotly "dash" codes.
lty2dash <- c(numeric.lty, named.lty, coded.lty)

aesConverters <-
  list(linetype=function(lty){
         lty2dash[as.character(lty)]
       },
       colour=function(col){
         toRGB(col)
       },
       size=identity,
       sizeref=identity,
       sizemode=identity,
       alpha=identity,
       shape=function(pch){
         pch2symbol[as.character(pch)]
       },
       direction=identity)

toBasic <- list(
  segment=function(g){
    ## Every row is one segment, we convert to a line with several
    ## groups which can be efficiently drawn by adding NA rows.
    g$data$group <- 1:nrow(g$data)
    used <- c("x", "y", "xend", "yend")
    others <- g$data[!names(g$data) %in% used]
    g$data <- with(g$data, {
      rbind(cbind(x, y, others),
            cbind(x=xend, y=yend, others))
    })
    group2NA(g, "path")
  },
  polygon=function(g){
    if(is.null(g$params$fill)){
      g
    }else if(is.na(g$params$fill)){
      group2NA(g, "path")
    }else{
      g
    }
  },
  path=function(g){
    group2NA(g, "path")
  },
  line=function(g){
    g$data <- g$data[order(g$data$x),]
    group2NA(g, "path")
  },
  boxplot=function(g) {
    g$data <- g$prestats.data
    g
  },
  bar=function(g) {
    g$prestats.data$fill <- g$data$fill[match(g$prestats.data$group, g$data$group)]
    g$params$xstart <- min(g$data$xmin)
    g$params$xend <- max(g$data$xmax)
    g$data <- g$prestats.data
    g
  },
  contour=function(g) {
    g$data <- g$prestats.data
    g
  },
  density2d=function(g) {
    g$data <- g$prestats.data
    g
  },
  abline=function(g) {
    g$params$xstart <- min(g$prestats.data$globxmin)
    g$params$xend <- max(g$prestats.data$globxmax)
    g
  },
  point=function(g) {
    if ("size" %in% names(g$data)) {
      g$params$sizemin <- min(g$prestats.data$globsizemin)
      g$params$sizemax <- max(g$prestats.data$globsizemax)
    }
    g
  }
)


#' Convert basic geoms to traces.
geom2trace <- list(
  path=function(data, params){
    list(x=data$x,
         y=data$y,
         name=params$name,
         text=data$text,
         type="scatter",
         mode="lines",
         line=paramORdefault(params, aes2line, line.defaults))
  },
  polygon=function(data, params){
    list(x=c(data$x, data$x[1]),
         y=c(data$y, data$y[1]),
         name=params$name,
         text=data$text,
         type="scatter",
         mode="lines",
         line=paramORdefault(params, aes2line, line.defaults),
         fill="tonextx",
         fillcolor=toRGB(params$fill))
  },
  point=function(data, params){
    L <- list(x=data$x,
              y=data$y,
              name=params$name,
              text=data$text,
              type="scatter",
              mode="markers",
              marker=paramORdefault(params, aes2marker, marker.defaults))
    if("size" %in% names(data)){
      L$text <- paste("size:", data$size)
      L$marker$sizeref <- default.marker.sizeref
      ## Make sure sizes are passed as a list even when there is only one element.
      s <- data$size
      marker.size <- 5 * (s - params$sizemin)/(params$sizemax - params$sizemin) + 0.25
      marker.size <- marker.size * marker.size.mult
      L$marker$size <- if (length(s) > 1) marker.size else list(marker.size)
      L$marker$line$width <- 0
    }
    L
  },
  text=function(data, params){
    L <- list(x=data$x,
              y=data$y,
              text=data$label,
              type="scatter",
              mode="text")
    if (!is.null(params$size)) {
      L$textfont$size <- params$size
    }
    if (!is.null(data$colour)) {
      L$textfont$color <- data$colour
    }
    L
  },
  bar=function(data, params) {
    L <- list(x=data$x,
              name=params$name,
              text=data$text,
              marker=list(color=toRGB(params$fill)))

    if (!is.null(params$colour)) {
      L$marker$line <- list(color=toRGB(params$colour))
      L$marker$line$width <- if (is.null(params$size)) 1 else params$size
    }
    
    if (!is.null(params$alpha))
      L$opacity <- params$alpha
    
    if (params$stat.type == "bin") {
      L$type <- "histogram"
      if (is.null(params$binwidth)) {
        L$autobinx <- TRUE
      } else {
        L$autobinx <- FALSE
        L$xbins=list(start=params$xstart,
          end=params$xend,
          size=params$binwidth)
      }
    } else {
      L$y <- data$y
      L$type <- "bar"
    }
    L
  },
  step=function(data, params) {
    list(x=data$x,
         y=data$y,
         name=params$name,
         type="scatter",
         mode="lines",
         line=paramORdefault(params, aes2line, line.defaults))
  },
  tile=function(data, params) {
    list(x=unique(data$x),
         y=unique(data$y),
         z=t(matrix(data$fill.name, nrow=length(unique(data$x)),
                    ncol=length(unique(data$y)))),
         name=params$name,
         type="heatmap",
         mode="lines",
         line=paramORdefault(params, aes2line, line.defaults))
  },
  boxplot=function(data, params) {
    list(y=data$y,
         name=params$name,
         type="box")
  },
  contour=function(data, params) {
    L <- list(x=unique(data$x),
              y=unique(data$y),
              z=t(matrix(data$z, nrow=length(unique(data$x)),
                         ncol=length(unique(data$y)))),
              name=params$name,
              type="contour",
              line=paramORdefault(params, aes2line, line.defaults))
    L$contours=list(coloring="lines")
    L
  },
  density2d=function(data, params) {
    L <- list(x=data$x,
              y=data$y,
              name=params$name,
              type="histogram2dcontour",
              line=paramORdefault(params, aes2line, line.defaults))
    L$contours=list(coloring="lines")
    L
  },
  errorbar=function(data, params) {
    list(x=data$x,
         y=data$y,
         error_y=list(arrayminus=data$y-data$ymin,
                      array=data$ymax-data$y,
                      color=toRGB(data$colour)))
  },
  errorbarh=function(data, params) {
    list(x=data$x,
         y=data$y,
         error_x=list(arrayminus=data$x-data$xmin,
                      array=data$xmax-data$x,
                      color=toRGB(data$colour)))
  },
  area=function(data, params) {
    list(x=c(data$x[1], data$x, tail(data$x, n=1)),
         y=c(0, data$y, 0), 
         name=params$name,
         type="scatter",
         fill="tozeroy")
  },
  ribbon=function(data, params) {
    list(x=c(data$x[1], data$x, rev(data$x)),
         y=c(data$ymin[1], data$ymax, rev(data$ymin)),
         type="scatter",
         fill="tonexty")
  },
  abline=function(data, params) {
    list(x=c(params$xstart, params$xend),
         y=c(params$intercept + params$xstart * params$slope,
             params$intercept + params$xend * params$slope),
      name=params$name,
      type="scatter",
      mode="lines",
      line=paramORdefault(params, aes2line, line.defaults))
  }
)


#' Convert ggplot2 aes to line parameters.
aes2line <- c(linetype="dash",
              colour="color",
              size="width",
              direction="shape")

markLegends <-
  ## NOTE: Do we also want to split on size?
  ## Legends based on sizes not implemented yet in Plotly
  ##  list(point=c("colour", "fill", "shape", "size"),
  list(point=c("colour", "fill", "shape"),
       path=c("linetype", "size", "colour"),
       polygon=c("colour", "fill", "linetype", "size", "group"),
       bar=c("colour", "fill"),
       step=c("linetype", "size", "colour"),
       boxplot=c("x"))

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
  ## Always use identity size scale so that plot.ly gets the real
  ## units for the size variables.
  p <- tryCatch({
    ## this will be an error for discrete variables.
    suppressMessages({
      ggplot_build(p+scale_size_continuous())
      p+scale_size_identity()
    })
  },error=function(e){
    p
  })
  layout <- list()
  trace.list <- list()
  
  ## Before building the ggplot, we would like to add aes(name) to
  ## figure out what the object group is later. This also copies any
  ## needed global aes/data values to each layer, so we do not have to
  ## worry about combining global and layer-specific aes/data later.
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
  
  ## Extract data from built ggplots
  built <- ggplot_build2(p)
  
  # Get global x-range now because we need some of its info in layer2traces
  ggranges <- built$panel$ranges
  # Extract x.range
  xrange <- sapply(ggranges, `[[`, "x.range", simplify=FALSE, USE.NAMES=FALSE)
  ggxmin <- min(sapply(xrange, min))
  ggxmax <- max(sapply(xrange, max))
  
  # Get global size range because we need some of its info in layer2traces
  if ("size.name" %in% name.names) {
    sizerange <- sapply(built$prestats.data, `[[`, "size")
    ggsizemin <- min(unlist(sizerange))
    ggsizemax <- max(unlist(sizerange))
  }
  
  for(i in seq_along(built$plot$layers)){
    ## This is the layer from the original ggplot object.
    L <- p$layers[[i]]
    
    ## for each layer, there is a correpsonding data.frame which
    ## evaluates the aesthetic mapping.
    df <- built$data[[i]]
    
    ## Test fill and color to see if they encode a quantitative
    ## variable. This may be useful for several reasons: (1) it is
    ## sometimes possible to plot several different colors in the same
    ## trace (e.g. points), and that is faster for large numbers of
    ## data points and colors; (2) factors on x or y axes should be
    ## sent to plotly as characters, not as numeric data (which is
    ## what ggplot_build gives us).
    misc <- list()
    for(a in c("fill", "colour", "x", "y")){
      for(data.type in c("continuous", "date", "datetime", "discrete")){
        fun.name <- sprintf("scale_%s_%s", a, data.type)
        misc.name <- paste0("is.", data.type)
        misc[[misc.name]][[a]] <- tryCatch({
          fun <- get(fun.name)
          suppressMessages({
            with.scale <- p+fun()
          })
          ggplot_build(with.scale)
          TRUE
        }, error=function(e){
          FALSE
        })
      }
    }
    
    ## scales are needed for legend ordering.
    for(sc in p$scales$scales){
      a <- sc$aesthetics
      if(length(a) == 1){
        br <- sc$breaks
        ranks <- seq_along(br)
        names(ranks) <- br
        misc$breaks[[sc$aesthetics]] <- ranks
      }
    }
    
    ## get gglayout now because we need some of its info in layer2traces
    gglayout <- built$panel$layout
    ## invert rows so that plotly and ggplot2 show panels in the same order
    gglayout$plotly.row <- max(gglayout$ROW) - gglayout$ROW + 1
    
    ## Add ROW and COL to df: needed to link axes to traces; keep df's
    ## original ordering while merging.
    df$order <- seq_len(nrow(df))
    df <- merge(df, gglayout[, c("PANEL", "plotly.row", "COL")])
    df <- df[order(df$order),]
    df$order <- NULL

    misc$prestats.data <- merge(built$prestats.data[[i]],
                                gglayout[, c("PANEL", "plotly.row", "COL")])
    
    # Add global x-range info
    misc$prestats.data$globxmin <- ggxmin
    misc$prestats.data$globxmax <- ggxmax
    
    # Add global size info if relevant
    if ("size.name" %in% name.names) {
      misc$prestats.data$globsizemin <- ggsizemin
      misc$prestats.data$globsizemax <- ggsizemax
    }

    ## This extracts essential info for this geom/layer.
    traces <- layer2traces(L, df, misc)
    
    # Associate error bars with previous traces
    if (grepl("errorbar", L$geom$objname)) {
      for (j in 1:length(trace.list)) {
        temp <- list()
        ind <- traces[[1]]$x %in% trace.list[[j]]$x
        only_ind <- function(x) x[ind]
        if ("errorbarh" %in% L$geom$objname) {
          temp <- lapply(traces[[1]]$error_x, only_ind)
          # Colour of error bar has to be one string
          if (length(temp$color) > 1) temp$color <- temp$color[1]
          trace.list[[j]]["error_x"] <- list(temp)
        } else {
          temp <- lapply(traces[[1]]$error_y, only_ind)
          if (length(temp$color) > 1) temp$color <- temp$color[1]
          trace.list[[j]]["error_y"] <- list(temp)
        }
      }
    } else {
      # Do we really need to coord_transform?
      # g$data <- ggplot2:::coord_transform(built$plot$coord, g$data,
      #                                     built$panel$ranges[[1]])
      trace.list <- c(trace.list, traces)
    }
  }

  ## for barcharts, verify that all traces have the same barmode; we don't
  ## support different barmodes on the same plot yet.
  barmodes <- do.call(c, lapply(trace.list, function (x) x$barmode))
  barmodes <- barmodes[!is.null(barmodes)]
  if (length(barmodes) > 0) {    
    layout$barmode <- barmodes[1]
    if (!all(barmodes == barmodes[1]))
      warning(paste0("You have multiple barcharts or histograms with different positions; ",
                     "Plotly's layout barmode will be '", layout$barmode, "'."))
  }
  
  ## Export axis specification as a combination of breaks and labels, on
  ## the relevant axis scale (i.e. so that it can be passed into d3 on the
  ## x axis scale instead of on the grid 0-1 scale). This allows
  ## transformations to be used out of the box, with no additional d3
  ## coding.
  theme.pars <- ggplot2:::plot_theme(p)
  
  ## Flip labels if coords are flipped - transform does not take care
  ## of this. Do this BEFORE checking if it is blank or not, so that
  ## individual axes can be hidden appropriately, e.g. #1.
  ## ranges <- built$panel$ranges[[1]]
  ## if("flip"%in%attr(built$plot$coordinates, "class")){
  ##   temp <- built$plot$labels$x
  ##   built$plot$labels$x <- built$plot$labels$y
  ##   built$plot$labels$y <- temp
  ## }
  e <- function(el.name){
    ggplot2::calc_element(el.name, p$theme)
  }
  is.blank <- function(el.name, null.is.blank=FALSE) {
    ## NULL shows ticks and hides borders
    cls <- attr(e(el.name),"class")
    "element_blank" %in% cls || null.is.blank && is.null(cls)
  }
  for(xy in c("x","y")){
    ax.list <- list()
    s <- function(tmp)sprintf(tmp, xy)
    ax.list$tickcolor <- toRGB(theme.pars$axis.ticks$colour)
    ax.list$gridcolor <- toRGB(theme.pars$panel.grid.major$colour)
    ax.list$showgrid <- !is.blank(s("panel.grid.major.%s"))
    ## These numeric length variables are not easily convertible.
    ##ax.list$gridwidth <- as.numeric(theme.pars$panel.grid.major$size)
    ##ax.list$ticklen <- as.numeric(theme.pars$axis.ticks.length)
    ax.list$tickwidth <- theme.pars$axis.ticks$size
    tick.text.name <- s("axis.text.%s")
    ax.list$showticklabels <- !is.blank(tick.text.name)
    tick.text <- e(tick.text.name)
    ax.list$tickangle <- if(is.numeric(tick.text$angle)){
      -tick.text$angle
    }
    
    theme2font <- function(text){
      if(!is.null(text)){
        list(family=text$family,
             size=text$size,
             color=toRGB(text$colour))
      }
    }
    ## Translate axes labels.
    scale.i <- which(p$scales$find(xy))
    ax.list$title <- if(length(scale.i)){
      sc <- p$scales$scales[[scale.i]]
      if(!is.null(sc$name)){
        sc$name
      }else{
        p$labels[[xy]]
      }
    }else{
      p$labels[[xy]]
    }
    ax.list$tickfont <- theme2font(tick.text)
    title.text <- e(s("axis.title.%s"))
    ax.list$titlefont <- theme2font(title.text)
    ax.list$type <- if(misc$is.continuous[[xy]]){
      "linear"
    }else if(misc$is.discrete[[xy]]){
      "category"
    }else if(misc$is.date[[xy]] || misc$is.datetime[[xy]]){
      "date"
    }else{
      stop("unrecognized data type for ", xy, " axis")
    }
    ## Lines drawn around the plot border:
    ax.list$showline <- !is.blank("panel.border", TRUE)
    ax.list$linecolor <- toRGB(theme.pars$panel.border$colour)
    ax.list$linewidth <- theme.pars$panel.border$size
    ## Some other params that we used in animint but we don't yet
    ## translate to plotly:
    !is.blank(s("axis.line.%s"))
    !is.blank(s("axis.ticks.%s"))
    layout[[s("%saxis")]] <- ax.list
  }

  ## copy [x/y]axis to [x/y]axisN and set domain, range, etc. for each
  xaxis.title <- layout$xaxis$title
  yaxis.title <- layout$yaxis$title
  inner.margin <- 0.01 ## between facets
  outer.margin <- 0.05 ## to put titles outside of the plots
  orig.xaxis <- layout$xaxis
  orig.yaxis <- layout$yaxis
  if (nrow(gglayout) > 1)
    {
      row.size <- 1. / max(gglayout$ROW)
      col.size <- 1. / max(gglayout$COL)
      for (i in seq_len(nrow(gglayout)))
        {
          row <- gglayout[i, "plotly.row"] 
          col <- gglayout[i, "COL"] 
          x <- col * col.size
          xmin <- x - col.size
          xmax <- x - inner.margin
          y <- row * row.size
          ymin <- y - row.size
          ymax <- y - inner.margin
          if ("wrap" %in% class(p$facet))
            ymax <- ymax - 0.04
          yaxis.name <- if (row == 1) "yaxis" else paste0("yaxis", row)
          xaxis.name <- if (col == 1) "xaxis" else paste0("xaxis", col)
          layout[[xaxis.name]] <- orig.xaxis
          layout[[xaxis.name]]$domain <- c(xmin, xmax)
          layout[[xaxis.name]]$anchor <- "y"
          layout[[xaxis.name]]$title <- NULL
          if (orig.xaxis$type == "linear" && # range only makes sense for numeric data
              (is.null(p$facet$scales) || p$facet$scales == "fixed" || p$facet$scales == "free_y"))
            {
              layout[[xaxis.name]]$range <- built$panel$ranges[[i]]$x.range
              layout[[xaxis.name]]$autorange <- FALSE
            }

          layout[[yaxis.name]] <- orig.yaxis
          layout[[yaxis.name]]$domain <- c(ymin, ymax)
          layout[[yaxis.name]]$anchor <- "x"
          layout[[yaxis.name]]$title <- NULL
          if (orig.yaxis$type == "linear" && # range only makes sense for numeric data
              (is.null(p$facet$scales) || p$facet$scales == "fixed" || p$facet$scales == "free_x"))
            {
              layout[[yaxis.name]]$range <- built$panel$ranges[[i]]$y.range
              layout[[yaxis.name]]$autorange <- FALSE
            }

        }
      ## add panel titles as annotations
      annotations <- list()
      nann <- 1
      make.label <- function(text, x, y, xanchor="auto", yanchor="auto", textangle=0)
        list(text=text, showarrow=FALSE, x=x, y=y, ax=0, ay=0, 
             xref="paper", yref="paper", xanchor=xanchor, yanchor=yanchor, 
             textangle=textangle)
      
      if ("grid" %in% class(p$facet))
        {
          frows <- names(p$facet$rows)
          nann <- 1
          
          for (i in seq_len(max(gglayout$ROW)))
            {
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
          for (i in seq_len(max(gglayout$COL)))
            {
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

          ## add empty traces everywhere so that the background shows even if there
          ## is no data for a facet
          for (r in seq_len(max(gglayout$ROW)))
            for (c in seq_len(max(gglayout$COL)))
              trace.list <- c(trace.list, list(list(xaxis=paste0("x", c), yaxis=paste0("y", r), showlegend=FALSE)))
        }
      else if ("wrap" %in% class(p$facet))
        {
          facets <- names(p$facet$facets)
          for (i in seq_len(max(as.numeric(gglayout$PANEL))))
            {
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

      ## axes titles
      annotations[[nann]] <- make.label(xaxis.title, 
                                        0.5, 
                                        -outer.margin,
                                        yanchor="top")
      nann <- nann + 1
      annotations[[nann]] <- make.label(yaxis.title, 
                                        -outer.margin, 
                                        0.5,
                                        textangle=-90)
      nann <- nann + 1
      
      layout$annotations <- annotations
    }
  
  ## Remove legend if theme has no legend position
  layout$showlegend <- !(theme.pars$legend.position=="none")
  
  ## Main plot title.
  layout$title <- built$plot$labels$title
  
  ## Background color.
  layout$plot_bgcolor <- toRGB(theme.pars$panel.background$fill)
  layout$paper_bgcolor <- toRGB(theme.pars$plot.background$fill)
  
  ## Legend.
  layout$margin$r <- 10
  if (exists("increase_margin_r")) {
    layout$margin$r <- 60
  }
  layout$legend <- list(bordercolor="transparent", x=100, y=1/2)
  
  ## Family font for text
  if (!is.null(theme.pars$text$family)) {
    layout$titlefont$family   <- theme.pars$text$family
    layout$legend$font$family <- theme.pars$text$family
  }
  
  ## Family font for title
  if (!is.null(theme.pars$plot.title$family)) {
    layout$titlefont$family <- theme.pars$plot.title$family
  }
  
  ## Family font for legend
  if (!is.null(theme.pars$legend.text$family)) {
    layout$legend$font$family <- theme.pars$legend.text$family
  }
  
  ## Bold, italic and bold.italic face for text
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
  
  ## Bold, italic and bold.italic face for title
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
  
  ## Bold, italic, and bold.italic face for axis title
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
  
  trace.list$kwargs <- list(layout=layout)
  if(length(trace.list) < 2){
    stop("No exportable traces")
  }
  
  trace.list
}

#' Convert a layer to a list of traces. Called from gg2list()
#' @param l one layer of the ggplot object
#' @param d one layer of calculated data from ggplot2::ggplot_build(p)
#' @param misc named list.
#' @return list representing a layer, with corresponding aesthetics, ranges, and groups.
#' @export
layer2traces <- function(l, d, misc) {
  g <- list(geom=l$geom$objname,
            data=d,
            prestats.data=misc$prestats.data)
  ## needed for when group, etc. is an expression.
  g$aes <- sapply(l$mapping, function(k) as.character(as.expression(k)))
  # Partial conversion for geom_violin (Plotly does not offer KDE yet)
  if (g$geom == "violin") {
    g$geom <- "boxplot"
    warning("Converting violin plot into boxplot:\n
            probability density estimation is not supported in Plotly yet.")
  }

  ## Barmode.
  barmode <- "group"
  if (g$geom == "bar" || g$geom == "histogram") {
    if (l$stat$objname == "bin" && g$geom != "histogram") {
      warning("You may want to use geom_histogram.")
    }
    g$geom <- "bar"  # histogram is just an alias for geom_bar + stat_bin
    pos <- l$position$.super$objname
    if (pos == "identity") {
      barmode <- "overlay"
    } else if (pos == "stack") {
      barmode <- "stack"
    }
  }

  ## For non-numeric data on the axes, we should take the values from
  ## the original data.
  for (axis.name in c("x", "y")){
    if (!misc$is.continuous[[axis.name]]){
      aes.names <- paste0(axis.name, c("", "end", "min", "max"))
      aes.used <- aes.names[aes.names %in% names(g$aes)]
      for(a in aes.used) {
        col.name <- g$aes[aes.used]
        data.vec <- l$data[[col.name]]
        
        # For some plot types, we overwrite `data` with `prestats.data`.
        pdata.vec <- misc$prestats.data[[a]]
        if (inherits(data.vec, "POSIXt")) {
          ## Re-create dates from nb seconds
          data.vec <- try(strftime(as.POSIXlt(g$data[[a]], origin=the.epoch),
                                   "%Y-%m-%d %H:%M:%S"), silent=TRUE)
          pdata.vec <- strftime(as.POSIXlt(g$prestats.data[[a]],
                                           origin=the.epoch),
                                "%Y-%m-%d %H:%M:%S")
        } else if (inherits(data.vec, "Date")) {
          ## Re-create dates from nb days
          data.vec <- try(strftime(as.Date(g$data[[a]], origin=the.epoch),
                                   "%Y-%m-%d %H:%M:%S"), silent=TRUE)
          pdata.vec <- strftime(as.Date(g$prestats.data[[a]], origin=the.epoch),
                                "%Y-%m-%d %H:%M:%S")
        } else if (inherits(data.vec, "factor")) {
          ## Re-order data so that Plotly gets it right from ggplot2.
          g$data <- g$data[order(g$data[[a]]),]
          data.vec <- data.vec[match(g$data[[a]], as.numeric(data.vec))]
          g$prestats.data <- g$prestats.data[order(g$prestats.data[[a]]),]
          pdata.vec <- pdata.vec[match(g$prestats.data[[a]],
                                       as.numeric(pdata.vec))]
          if (length(pdata.vec) == length(data.vec))
            pdata.vec <- data.vec
        }
        g$data[[a]] <- data.vec
        g$prestats.data[[a]] <- pdata.vec
      }
    }
  }
  ## use un-named parameters so that they will not be exported
  ## to JSON as a named object, since that causes problems with
  ## e.g. colour.
  g$params <- c(l$geom_params, l$stat_params)
  ## non-ggplot2 params like name are useful for plot.ly and ggplot2
  ## places them into stat_params.
  for(p.name in names(g$params)){
    ## c("foo") is translated to "foo" in JSON, so instead we use
    ## list("foo") which becomes ["foo"]. However we need to make sure
    ## that the list does not have names since list(bar="foo") becomes
    ## {"bar":"foo"}
    names(g$params[[p.name]]) <- NULL
  }
  
  ## Convert complex ggplot2 geoms so that they are treated as special
  ## cases of basic geoms. In ggplot2, this processing is done in the
  ## draw method of the geoms.
  
  ## Every plotly trace has one of these types
  ## type=scatter,bar,box,histogramx,histogram2d,heatmap
  
  ## for type=scatter, you can define
  ## mode=none,markers,lines,lines+markers where "lines" is the
  ## default for 20 or more points, "lines+markers" is the default for
  ## <20 points. "none" is useful mainly if fill is used to make area
  ## plots with no lines.
  
  ## marker=list(size,line,color="rgb(54,144,192)",opacity,symbol)
  
  ## symbol=circle,square,diamond,cross,x,
  ## triangle-up,triangle-down,triangle-left,triangle-right

  ## First convert to a "basic" geom, e.g. segments become lines.
  convert <- toBasic[[g$geom]]
  basic <- if(is.null(convert)){
    g
  }else{
    convert(g)
  }
  ## Then split on visual characteristics that will get different
  ## legend entries.
  data.list <- if(basic$geom %in% names(markLegends)){
    mark.names <- markLegends[[basic$geom]]
    ## However, continuously colored points are an exception: they do
    ## not need a legend entry, and they can be efficiently rendered
    ## using just 1 trace.
    
    ## Maybe it is nice to show a legend for continuous points?
    ## if(basic$geom == "point"){
    ##   to.erase <- names(misc$is.continuous)[misc$is.continuous]
    ##   mark.names <- mark.names[!mark.names %in% to.erase]
    ## }
    name.names <- sprintf("%s.name", mark.names)
    ## split on 'PANEL' to support facets
    is.split <- names(basic$data) %in% c(name.names, "PANEL")
    if(any(is.split)){
      data.i <- which(is.split)
      matched.names <- names(basic$data)[data.i]
      name.i <- name.names %in% matched.names
      invariable.names <- cbind(name.names, mark.names)[name.i,]
      other.names <- !names(basic$data) %in% invariable.names
      vec.list <- basic$data[is.split]
      df.list <- split(basic$data, vec.list, drop=TRUE)
      lapply(df.list, function(df){
        params <- basic$params
        params[invariable.names] <- if (ncol(x <- df[1, invariable.names]) > 0) x else NULL
        list(data=df[other.names],
             params=params)
      })
    }
  }

  ## case of no legend, if either of the two ifs above failed.
  if(is.null(data.list)){
    data.list <- structure(list(list(data=basic$data, params=basic$params)),
                           names=basic$params$name)
  }
  getTrace <- geom2trace[[basic$geom]]
  if(is.null(getTrace)){
    warning("Conversion not implemented for geom_",
            g$geom, " (basic geom_", basic$geom, "), ignoring. ",
            "Please open an issue with your example code at ",
            "https://github.com/ropensci/plotly/issues")
    return(list())
  }
  traces <- NULL
  names.in.legend <- NULL
  for(data.i in seq_along(data.list)){
    data.params <- data.list[[data.i]]
    data.params$params$stat.type <- l$stat$objname
    tr <- do.call(getTrace, data.params)
    for (v.name in c("x", "y")) {
      vals <- tr[[v.name]]
      if (length(vals) > 0 && is.na(vals[length(vals)])) {
        tr[[v.name]] <- vals[-length(vals)]
      }
    }
    name.names <- grep("[.]name$", names(data.params$params), value=TRUE)
    if(length(name.names)){
      for(a.name in name.names){
        a <- sub("[.]name$", "", a.name)
        a.value <- as.character(data.params$params[[a.name]])
        ranks <- misc$breaks[[a]]
        if(length(ranks)){
          tr$sort[[a.name]] <- ranks[[a.value]]
        }
      }
      name.list <- data.params$params[name.names]
      tr$name <- paste(unlist(name.list), collapse=".")
    }

    dpd <- data.params$data
    if ("PANEL" %in% names(dpd) && nrow(dpd) > 0)
      {
        tr$xaxis <- paste0("x", dpd[1, "COL"])
        tr$yaxis <- paste0("y", dpd[1, "plotly.row"])
      }
    
    if (is.null(tr$name) || tr$name %in% names.in.legend)
        tr$showlegend <- FALSE
    names.in.legend <- c(names.in.legend, tr$name)
    
    if (g$geom == "bar")
      tr$barmode <- barmode
    traces <- c(traces, list(tr))
  }

  sort.val <- sapply(traces, function(tr){
    rank.val <- unlist(tr$sort)
    if(is.null(rank.val)){
      0
    }else if(length(rank.val)==1){
      rank.val
    }else{
      0
    }
  })

  ord <- order(sort.val)
  no.sort <- traces[ord]
  for(tr.i in seq_along(no.sort)){
    no.sort[[tr.i]]$sort <- NULL
  }
  no.sort
}

##' convert ggplot params to plotly.
##' @param params named list ggplot names -> values.
##' @param aesVec vector mapping ggplot names to plotly names.
##' @param defaults named list ggplot names -> values.
##' @export
##' @return named list.
##' @author Toby Dylan Hocking
paramORdefault <- function(params, aesVec, defaults){
  marker <- list()
  for(ggplot.name in names(aesVec)){
    plotly.name <- aesVec[[ggplot.name]]
    ggplot.value <- params[[ggplot.name]]
    if(is.null(ggplot.value)){
      ggplot.value <- defaults[[ggplot.name]]
    }
    if(is.null(ggplot.value)){
      stop("no ggplot default for ", ggplot.name)
    }
    convert <- aesConverters[[ggplot.name]]
    if(is.null(convert)){
      stop("no ggplot converter for ", ggplot.name)
    }
    plotly.value <- convert(ggplot.value)
    names(plotly.value) <- NULL
    marker[[plotly.name]] <- plotly.value
  }
  marker
}

#' Convert R colors to RGB hexadecimal color values
#' @param x character
#' @return hexadecimal color value (if is.na(x), return "none" for compatibility with JavaScript)
#' @export
toRGB <- function(x){
  if(is.null(x))return(x)
  rgb.matrix <- col2rgb(x)
  rgb.text <- apply(rgb.matrix, 2, paste, collapse=",")
  rgb.css <- sprintf("rgb(%s)", rgb.text)
  ifelse(is.na(x), "none", rgb.css)
}

