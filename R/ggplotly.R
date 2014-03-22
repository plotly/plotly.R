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
                pch="symbol",
                colour="color",
                size="size",
                ##TODO="line", ## line color, size, and dash
                shape="symbol",
                text="text")
marker.defaults <- c(alpha=1,
                     shape="o",
                     pch="o",
                     colour="black")
#' Convert ggplot2 aes to line parameters.
aes2line <- c(linetype="dash",
              colour="color",
              size="width",
              text="text")
line.defaults <-
  list(linetype="solid",
       colour="black",
       size=2)

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

#' Convert a ggplot to a list.
#' @import ggplot2
#' @param p ggplot2 plot.
#' @return list representing a ggplot.
#' @export
gg2list <- function(p){
  ## Always use identity size scale so that plot.ly gets the real
  ## units for the size variables.
  p <- p+scale_size_identity()
  layout <- list()
  trace.list <- list()
  ## Before building the ggplot, we would like to add aes(name) to
  ## figure out what the object group is later.
  for(layer.i in seq_along(p$layers)){
    a <- c(p$layers[[layer.i]]$mapping, p$mapping)
    group.vars <- c("colour", "color", "col",
                    "fill",
                    "linetype", "lty",
                    "shape", "pch")
    group.var <- a$name
    for(gv in group.vars){
      if(is.null(group.var)){
        g.expr <- a[[gv]]
        if(!is.null(g.expr)){
          group.var <- g.expr
        }
      }
    }
    p$layers[[layer.i]]$mapping$name <- group.var
  }
  ## Extract data from built ggplots 
  built <- ggplot2::ggplot_build(p)
  ranges <- built$panel$ranges[[1]]
  for(i in seq_along(built$plot$layers)){
    ## This is the layer from the original ggplot object.
    L <- built$plot$layers[[i]]

    ## for each layer, there is a correpsonding data.frame which
    ## evaluates the aesthetic mapping.
    df <- built$data[[i]]

    ## This extracts essential info for this geom/layer.
    traces <- layer2traces(L, df, ranges)

    ## Do we really need to coord_transform?
    ##g$data <- ggplot2:::coord_transform(built$plot$coord, g$data,
    ##                                     built$panel$ranges[[1]])
    trace.list <- c(trace.list, traces)
  }
  # Export axis specification as a combination of breaks and
  # labels, on the relevant axis scale (i.e. so that it can
  # be passed into d3 on the x axis scale instead of on the 
  # grid 0-1 scale). This allows transformations to be used 
  # out of the box, with no additional d3 coding. 
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
  is.blank <- function(el.name){
    x <- ggplot2::calc_element(el.name, p$theme)
    "element_blank"%in%attr(x,"class")
  }
  for(xy in c("x","y")){
    ax.list <- list()
    s <- function(tmp)sprintf(tmp, xy)
    ax.list$tickcolor <- toRGB(theme.pars$axis.ticks$colour)
    ax.list$gridcolor <- toRGB(theme.pars$panel.grid.major$colour)
    ## Some other params that we used in animint but we don't yet
    ## translate to plotly:
    ranges[[s("%s.major")]]
    ranges[[s("%s.labels")]]
    ranges[[s("%s.range")]]
    built$plot$labels[[xy]]
    !is.blank(s("axis.line.%s"))
    !is.blank(s("axis.ticks.%s"))
    layout[[s("%saxis")]] <- ax.list
  }
  
  ## Remove legend if theme has no legend position
  if(theme.pars$legend.position=="none") layout$showlegend <- FALSE

  ## Main plot title.
  layout$title <- built$plot$labels$title

  ## Background color.
  layout$plot_bgcolor <- toRGB(theme.pars$panel.background$fill)
  layout$paper_bgcolor <- toRGB(theme.pars$plot.background$fill)

  trace.list$kwargs <- list(layout=layout)
  trace.list
}

#' Convert a layer to a list of traces. Called from gg2list()
#' @param l one layer of the ggplot object
#' @param d one layer of calculated data from ggplot2::ggplot_build(p)
#' @param ranges axes ranges
#' @return list representing a layer, with corresponding aesthetics, ranges, and groups.
#' @export
layer2traces <- function(l, d, ranges){
  g <- list(geom=l$geom$objname,
            data=d)
  g$aes <- sapply(l$mapping, function(k) as.character(as.expression(k))) # needed for when group, etc. is an expression

  ## use un-named parameters so that they will not be exported
  ## to JSON as a named object, since that causes problems with
  ## e.g. colour.
  g$params <- c(l$geom_params, l$stat_params)
  ## non-ggplot2 params like name are useful for plot.ly and ggplot2
  ## places them into stat_params.
  for(p.name in names(g$params)){
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

  geom <- function(...){
    gnames <- c(...)
    g$geom %in% gnames
  }
  g$geom <- if(geom("abline")){
    # "Trick" ggplot coord_transform into transforming the slope and intercept
    g$data[,"x"] <- ranges$x.range[1]
    g$data[,"xend"] <- ranges$x.range[2]
    g$data[,"y"] <- g$data$slope*ranges$x.range[1]+g$data$intercept
    g$data[,"yend"] <-  g$data$slope*ranges$x.range[2]+g$data$intercept
    g$data <- as.data.frame(g$data)
    if(g$aes[["group"]]=="1"){ 
      # ggplot2 defaults to adding a group attribute
      # which misleads for situations where there are 
      # multiple lines with the same group. 
      # if the group attribute conveys no additional 
      # information, remove it.
      ## TODO: Figure out a better way to handle this...
      g$aes <- g$aes[-which(names(g$aes)=="group")]
    } 
    "segment"
  } else if(geom("point")){
    g$data$group <- 1
    # Fill set to match ggplot2 default of filled in circle. 
    if(!"fill"%in%names(g$data) & "colour"%in%names(g$data)){
      g$data[["fill"]] <- g$data[["colour"]]
    }
    "point"
  } else if(geom("ribbon")){
    # Color set to match ggplot2 default of fill with no outside border.
    if("fill"%in%names(g$data) & !"colour"%in%names(g$data)){
      g$data[["colour"]] <- g$data[["fill"]]
    }
    "ribbon"
  } else if(geom("density") | geom("area")){
    "ribbon"
  } else if(geom("tile") | geom("raster") | geom("histogram") ){
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g$data) & "fill"%in%names(g$data)){
      g$data[["colour"]] <- g$data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g$data)) g$data[["size"]] <- 0 
    }
    "rect"
  } else if(geom("bar")){
    "rect"
  } else if(g$geom=="bin2d"){
    stop("TODO")
  } else if(geom("boxplot")){
    stop("boxplots are not supported. Workaround: rects, lines, and points")
    ## TODO: boxplot support. But it is hard since boxplots are drawn
    ## using multiple geoms and it is not straightforward to deal with
    ## that using our current JS code. There is a straightforward
    ## workaround: combine working geoms (rects, lines, and points).

    g$data$outliers <- sapply(g$data$outliers, FUN=paste, collapse=" @ ") 
    # outliers are specified as a list... 
  } else if(geom("violin")){
    x <- g$data$x
    vw <- g$data$violinwidth
    xmin <- g$data$xmin
    xmax <- g$data$xmax
    g$data$xminv <- x-vw*(x-xmin)
    g$data$xmaxv <- x+vw*(xmax-x)
    newdata <- ddply(g$data, .(group), function(df){
      rbind(arrange(transform(df, x=xminv), y), arrange(transform(df, x=xmaxv), -y))
                })
    newdata <- ddply(newdata, .(group), function(df) rbind(df, df[1,]))
    g$data <- newdata
    "polygon"
  } else if(geom("step")){
    datanames <- names(g$data)
    g$data <- ddply(g$data, .(group), function(df) ggplot2:::stairstep(df))
    "path"
  } else if(geom("contour") | g$geom=="density2d"){
    g$aes[["group"]] <- "piece"
    "path"
  } else if(geom("freqpoly")){
    "line"
  } else if(geom("quantile")){
    "path"
  } else if(geom("hex")){
    ## TODO: for interactivity we will run into the same problems as
    ## we did with histograms. Again, if we put several
    ## clickSelects/showSelected values in the same hexbin, then
    ## clicking/hiding hexbins doesn't really make sense. Need to stop
    ## with an error if showSelected/clickSelects is used with hex.
    g$aes[["group"]] <- "group"
    dx <- ggplot2::resolution(g$data$x, FALSE)
    dy <- ggplot2::resolution(g$data$y, FALSE) / sqrt(3) / 2 * 1.15
    hex <- as.data.frame(hexcoords(dx, dy))[,1:2]
    hex <- rbind(hex, hex[1,]) # to join hexagon back to first point
    g$data$group <- as.numeric(interaction(g$data$group, 1:nrow(g$data)))
    ## this has the potential to be a bad assumption - 
    ##   by default, group is identically 1, if the user 
    ##   specifies group, polygons aren't possible to plot
    ##   using d3, because group will have a different meaning
    ##   than "one single polygon".
    newdata <- ddply(g$data, .(group), function(df){
      df$xcenter <- df$x
      df$ycenter <- df$y
      cbind(x=df$x+hex$x, y=df$y+hex$y, df[,-which(names(df)%in%c("x", "y"))])
    })
    g$data <- newdata
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g$data) & "fill"%in%names(g$data)){
      g$data[["colour"]] <- g$data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g$data)) g$data[["size"]] <- 0 
    }
    "polygon"
  } else if(geom("polygon", "line", "segment")) {
    ## all other geoms are basic, and keep the same name.
    g$geom
  } else {
    stop("unsupported geom ", g$geom)
  }

  ## For ggplot2 polygons, change convert groups to vectors with NA.
  if(geom("polygon")){
    poly.list <- split(g$data, g$data$group)
    is.group <- names(g$data) == "group"
    poly.na.df <- data.frame()
    for(i in seq_along(poly.list)){
      no.group <- poly.list[[i]][,!is.group,drop=FALSE]
      poly.na.df <- rbind(poly.na.df, no.group, NA)
    }
    g$data <- poly.na.df
  }

  ## Check g$data for color/fill - convert to hexadecimal so JS can
  ## parse correctly.
  for(color.var in c("colour", "color", "fill")){
    if(color.var %in% names(g$data)){
      g$data[,color.var] <- toRGB(g$data[,color.var])
    }
  }

  if(any(g$data$size == 0, na.rm=TRUE)){
    warning(sprintf("geom_%s with size=0 will be invisible",g$geom))
  }

  group.vars <- c("group",
                  "color", "colour",
                  "fill") #TODO.
  group.var <- NULL
  found.groups <- 0
  for(gv in group.vars){
    if(is.null(group.var)){
      g.col <- g$data[[gv]]
      n.groups <- length(unique(g.col))
      if(n.groups > 1){
        group.var <- g.col
        found.groups <- n.groups
      }
    }
  }
  group.list <- if(found.groups){
    split(g$data, group.var)
  }else{
    list(g$data)
  }
  for(group.i in seq_along(group.list)){
    group.data <- group.list[[group.i]]
    tr <- group2trace(group.data, g$params, g$geom)
    if(is.null(tr$name)){
      tr$name <- group.data$name
    }
    tr$name <- as.character(tr$name[1])
    g$traces[[group.i]] <- tr
  }
  g$traces
}

getMarker <- function(df, params, aesConverter, defaults, only=NULL){
  marker <- list()
  for(name in names(aesConverter)){
    plotly.name <- aesConverter[[name]]
    take.from <- if(name %in% names(params)){
      params
    } else if(name %in% names(df)){
      df
    } else {
      defaults
    }
    take.from <- as.list(take.from)
    to.write <- take.from[[name]]
    ## if(is.null(to.write)){
    ##   print(take.from)
    ##   stop("undefined marker ", name)
    ## }
    marker[[plotly.name]] <- if(!is.null(only)){
      to.write[only]
    }else{
      to.write
    }
  }
  if(length(marker$size) > 1){
    marker$sizeref <- min(marker$size)
    marker$sizemode <- "area"
  }
  if("dash" %in% names(marker)){
    marker$dash <- lty2dash[[marker$dash]]
  }
  marker
}

##' Convert 1 ggplot2 group to 1 plotly trace.
##' @param df data.frame.
##' @param params list of defaults.
##' @param geom length 1 character.
##' @return a list to be passed to plotly().
##' @author Toby Dylan Hocking
group2trace <- function(df, params, geom){
  ## Add plotly type/mode info based on geom type.
  tr <- if(geom == "point"){
    marker <- getMarker(df, params, aes2marker, marker.defaults)
    list(type="scatter",
         mode="markers",
         marker=marker)
  }else if(geom %in% c("line", "polygon")){
    list(type="scatter",
         mode="lines",
         line=getMarker(df, params, aes2line, line.defaults, 1))
  }else{
    stop("group2trace does not support geom ", geom)
  }
  ## Copy data to output trace
  for(name in c("x", "y", "text", "name")){
    take.from <- if(name %in% names(df)){
      df
    }else if(name %in% names(params)){
      params
    }
    tr[[name]] <- take.from[[name]]
  }
  tr
}
#' Get legend information.
#' @import plyr
#' @param plistextra output from ggplot2::ggplot_build(p)
#' @return list containing information for each legend
#' @export
getLegendList <- function(plistextra){
  plot <- plistextra$plot
  scales <- plot$scales
  layers <- plot$layers
  default_mapping <- plot$mapping
  theme <- ggplot2:::plot_theme(plot)
  position <- theme$legend.position
  # by default, guide boxes are vertically aligned
  theme$legend.box <- if(is.null(theme$legend.box)) "vertical" else theme$legend.box
  
  # size of key (also used for bar in colorbar guide)
  theme$legend.key.width <- if(is.null(theme$legend.key.width)) theme$legend.key.size
  theme$legend.key.height <- if(is.null(theme$legend.key.height)) theme$legend.key.size
  # by default, direction of each guide depends on the position of the guide.
  theme$legend.direction <- if(is.null(theme$legend.direction)){
    if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
      switch(position[1], top =, bottom = "horizontal", left =, right = "vertical")
    else
      "vertical"
  }
  # justification of legend boxes
  theme$legend.box.just <-
    if(is.null(theme$legend.box.just)) {
      if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
        switch(position, bottom =, top = c("center", "top"), left =, right = c("left", "top"))
      else
        c("center", "center")
    } 
  
  position <- theme$legend.position
  guides <- plyr::defaults(plot$guides, guides(colour="legend", fill="legend"))
  labels <- plot$labels
  gdefs <- ggplot2:::guides_train(scales = scales, theme = theme, guides = guides, labels = labels)
  if (length(gdefs) != 0) {
    gdefs <- ggplot2:::guides_merge(gdefs)
    gdefs <- ggplot2:::guides_geom(gdefs, layers, default_mapping)
  } else (ggplot2:::zeroGrob())
  names(gdefs) <- sapply(gdefs, function(i) i$title)
  lapply(gdefs, getLegend)
}

#' Function to get legend information for each scale
#' @param mb single entry from ggplot2:::guides_merge() list of legend data
#' @return list of legend information, NULL if guide=FALSE.
getLegend <- function(mb){
  guidetype <- mb$name
  ## The main idea of legends:
  
  ## 1. Here in getLegend I export the legend entries as a list of
  ## rows that can be used in a data() bind in D3.

  ## 2. In add_legend in the JS code I create a <table> for every
  ## legend, and then I bind the legend entries to <tr>, <td>, and
  ## <svg> elements.
  geoms <- sapply(mb$geoms, function(i) i$geom$objname)
  cleanData <- function(data, key, geom, params){
    if(nrow(data)==0) return(data.frame()); # if no rows, return an empty df.
    if("guide"%in%names(params)){
      if(params[["guide"]]=="none") return(data.frame()); # if no guide, return an empty df
    } 
    data$order <- 1:nrow(data)
    data <- merge(data, key)
    data <- data[order(data$order),]
    if(!".label"%in%names(data)) return(data.frame()); # if there are no labels, return an empty df.
    if(nrow(data)==0) return(data.frame());
    data <- data[,which(colSums(!is.na(data))>0)] # remove cols that are entirely na
    if("colour"%in%names(data)) data[["colour"]] <- toRGB(data[["colour"]]) # color hex values
    if("fill"%in%names(data)) data[["fill"]] <- toRGB(data[["fill"]]) # fill hex values
    names(data) <- paste(geom, names(data), sep="") # aesthetics by geom
    names(data) <- gsub(paste(geom, ".", sep=""), "", names(data), fixed=TRUE) # label isn't geom-specific
    data
  }
  dataframes <- lapply(mb$geoms, function(i) cleanData(i$data, mb$key, i$geom$objname, i$params))
  dataframes <- dataframes[which(sapply(dataframes, nrow)>0)]
  # Check to make sure datframes is non-empty. If it is empty, return NULL.
  if(length(dataframes)>0) {
    data <- merge_recurse(dataframes)
  } else return(NULL)
  data <- lapply(nrow(data):1, function(i) as.list(data[i,]))
  if(guidetype=="none"){
    NULL
  } else{
    list(guide = guidetype, 
         geoms = geoms, 
         title = mb$title, 
         entries = data)
  }
}

#' Convert R colors to RGB hexadecimal color values
#' @param x character
#' @return hexadecimal color value (if is.na(x), return "none" for compatibility with JavaScript)
#' @export
toRGB <- function(x){
  rgb.matrix <- col2rgb(x)
  rgb.text <- apply(rgb.matrix, 2, paste, collapse=",")
  rgb.css <- sprintf("rgb(%s)", rgb.text)
  ifelse(is.na(x), "none", rgb.css)
}

#' Function to merge a list of data frames (from the reshape package)
#' @param dfs list of data frames
#' @param ... other arguments to merge
#' @return data frame of merged lists
merge_recurse = function (dfs, ...) 
{
  if (length(dfs) == 1) {
    dfs[[1]]
  }
  else if (length(dfs) == 2) {
    merge(dfs[[1]], dfs[[2]], all.x = TRUE, sort = FALSE, ...)
  }
  else {
    merge(dfs[[1]], Recall(dfs[-1]), all.x = TRUE, sort = FALSE, 
          ...)
  }
}

