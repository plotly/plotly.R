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
  
  ## Barmode and bargap
  barmode <- "group"
  if (g$geom == "bar" || g$geom == "histogram") {
    if (l$stat$objname == "bin") {
      bargap <- 0
      if (g$geom != "histogram") {
        warning("You may want to use geom_histogram.")
      }
    } else {
      bargap <- "default"
    }
    g$geom <- "bar"  # histogram is just an alias for geom_bar + stat_bin
    pos <- l$position$.super$objname
    if (pos == "identity") {
      barmode <- "overlay"
    } else if (pos == "stack") {
      barmode <- "stack"
    }
  }
  if (g$geom == "density") {
    bargap <- 0
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
    
    # Bar Gap
    if (exists("bargap")) {
      tr$bargap <- bargap
    }
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
