## calc. the epoch
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
ribbon.line.defaults$size <- 0

# Convert R lty line type codes to plotly "dash" codes.
lty2dash <- c(numeric.lty, named.lty, coded.lty)

aesConverters <- list(linetype=function(lty) {
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
                      direction=identity)

markLegends <-
  ## NOTE: Do we also want to split on size?
  ## Legends based on sizes not implemented yet in Plotly
  ##  list(point=c("colour", "fill", "shape", "size"),
  list(point=c("colour", "fill", "shape"),
       path=c("linetype", "size", "colour", "shape"),
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
    # Add global y-range info
    misc$prestats.data$globymin <- ggymin
    misc$prestats.data$globymax <- ggymax
    
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
    
    ## When gridlines are dotted or dashed:
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
    ## These numeric length variables are not easily convertible.
    ##ax.list$gridwidth <- as.numeric(theme.pars$panel.grid.major$size)
    ##ax.list$ticklen <- as.numeric(theme.pars$axis.ticks.length)
    
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
    
    ax.list$zeroline <- FALSE  # ggplot2 plots do not show zero lines
    # Lines drawn around the plot border.
    ax.list$showline <- !is.blank("panel.border", TRUE)
    ax.list$linecolor <- toRGB(theme.pars$panel.border$colour)
    ax.list$linewidth <- theme.pars$panel.border$size
    ## Some other params that we used in animint but we don't yet
    ## translate to plotly:
    !is.blank(s("axis.line.%s"))
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
