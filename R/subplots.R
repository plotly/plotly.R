#' View multiple plots in a single view
#' 
#' @param ... any number of plotly objects
#' @param nrows number of rows for laying out plots in a grid-like structure.
#' Only used if no domain is already specified.
#' @param widths relative width of each column on a 0-1 scale. By default all
#' columns have an equal relative width.
#' @param heights relative height of each row on a 0-1 scale. By default all
#' rows have an equal relative height.
#' @param margin either a single value or four values (all between 0 and 1).
#' If four values are provided, the first is used as the left margin, the second
#' is used as the right margin, the third is used as the top margin, and the
#' fourth is used as the bottom margin.
#' If a single value is provided, it will be used as all four margins. 
#' @param shareX should the x-axis be shared amongst the subplots?
#' @param shareY should the y-axis be shared amongst the subplots?
#' @param titleX should x-axis titles be retained?
#' @param titleY should y-axis titles be retained?
#' @param which_layout adopt the layout of which plot? If the default value of 
#' "merge" is used, layout options found later in the sequence of plots will 
#' override options found earlier in the sequence. This argument also accepts a 
#' numeric vector specifying which plots to consider when merging.
#' @return A plotly object
#' @export
#' @author Carson Sievert
#' @examples \dontrun{
#' p1 <- plot_ly(economics, x = date, y = uempmed, showlegend = F)
#' p2 <- plot_ly(economics, x = date, y = unemploy, showlegend = F)
#' subplot(p1, p2, p1, p2, nrows = 2)
#' }

subplot <- function(..., nrows = 1, widths = NULL, heights = NULL, margin = 0.02, 
                    shareX = FALSE, shareY = FALSE, titleX = shareX, 
                    titleY = shareY, which_layout = "merge") {
  # are the dots a list of plotly objects?
  dotz <- list(...)
  if (length(dotz) == 1 && is.list(dotz[[1]]) && !is.plotly(dotz[[1]])) {
    dotz <- dotz[[1]]
  }
  # build each plot
  plotz <- lapply(dotz, plotly_build)
  # ensure "axis-reference" trace attributes are properly formatted
  # TODO: should this go inside plotly_build()?
  plotz <- lapply(plotz, function(p) {
    p$data <- lapply(p$data, function(tr) {
      if (length(tr[["geo"]])) {
        tr[["geo"]] <- sub("^geo1$", "geo", tr[["geo"]][1]) %||% NULL
        tr[["xaxis"]] <- NULL
        tr[["yaxis"]] <- NULL
      } else {
        tr[["geo"]] <- NULL
        tr[["xaxis"]] <- sub("^x1$", "x", tr[["xaxis"]][1] %||% "x") 
        tr[["yaxis"]] <- sub("^y1$", "y", tr[["yaxis"]][1] %||% "y")
      }
      tr
    })
    p
  })
  # Are any traces referencing "axis-like" layout attributes that are missing?
  # If so, move those traces to a "new plot", and inherit layout attributes,
  # which makes this sort of thing possible:
  # https://plot.ly/r/map-subplots-and-small-multiples/
  plots <- list()
  for (i in seq_along(plotz)) {
    p <- plots[[i]] <- plotz[[i]]
    layoutAttrs <- c(names(p$layout), c("geo", "xaxis", "yaxis"))
    xTraceAttrs <- sub("^x", "xaxis", sapply(p$data, function(tr) tr[["geo"]] %||% tr[["xaxis"]]))
    yTraceAttrs <- sub("^y", "yaxis", sapply(p$data, function(tr) tr[["geo"]] %||% tr[["yaxis"]]))
    missingAttrs <- setdiff(c(xTraceAttrs, yTraceAttrs), layoutAttrs)
    # move to next iteration if trace references are complete
    if (!length(missingAttrs)) next
    # remove each "missing" trace from this plot
    missingTraces <- xTraceAttrs %in% missingAttrs | yTraceAttrs %in% missingAttrs
    plots[[i]]$data[missingTraces] <- NULL
    # move traces with "similar missingness" to a new plot
    for (j in missingAttrs) {
      newPlot <- list(
        data = p$data[xTraceAttrs %in% j | yTraceAttrs %in% j],
        layout = p$layout
      )
      # reset the anchors
      newPlot$data <- lapply(newPlot$data, function(tr) {
        for (k in c("geo", "xaxis", "yaxis")) {
          tr[[k]] <- sub("[0-9]+", "", tr[[k]]) %||% NULL
        }
        tr
      })
      plots <- c(plots, list(newPlot))
    }
  }
  # main plot objects
  traces <- lapply(plots, "[[", "data")
  layouts <- lapply(plots, "[[", "layout")
  shapes <- lapply(layouts, "[[", "shapes")
  annotations <- lapply(layouts, function(x) {
    # keep non axis title annotations
    axes <- vapply(x$annotations, function(a) identical(a$annotationType, "axis"), logical(1))
    x$annotations[!axes]
  })
  # collect axis objects (note a _single_ geo object counts a both an x and y)
  geoDomainDefault <- list(x = c(0, 1), y = c(0, 1))
  xAxes <- lapply(layouts, function(lay) {
    keys <- grep("^geo|^xaxis", names(lay), value = TRUE) %||% "xaxis"
    for (k in keys) {
      lay[[k]]$domain <- lay[[k]]$domain %||% if (grepl("^geo", k)) geoDomainDefault else c(0, 1)
    }
    lay[keys]
  })
  yAxes <- lapply(layouts, function(lay) {
    keys <- grep("^geo|^yaxis", names(lay), value = TRUE) %||% "yaxis"
    for (k in keys) {
      lay[[k]]$domain <- lay[[k]]$domain %||% if (grepl("^geo", k)) geoDomainDefault else c(0, 1)
    }
    lay[keys]
  })
  if (!titleX) {
    xAxes <- lapply(xAxes, function(ax) lapply(ax, function(y) { y$title <- NULL; y }))
  }
  if (!titleY) {
    yAxes <- lapply(yAxes, function(ax) lapply(ax, function(y) { y$title <- NULL; y }))
  }
  # number of x/y axes per plot
  xAxisN <- vapply(xAxes, length, numeric(1))
  yAxisN <- vapply(yAxes, length, numeric(1))
  # old -> new axis name dictionary
  ncols <- ceiling(length(plots) / nrows)
  xAxisID <- seq_len(sum(xAxisN))
  if (shareX) {
    if (length(unique(xAxisN)) > 1) {
      warning("Must have a consistent number of axes per 'subplot' to share them.")
    } else {
      xAxisID <- rep(rep(seq_len(ncols * unique(xAxisN)), length.out = length(plots)), unique(xAxisN))
    }
  }
  yAxisID <- seq_len(sum(yAxisN))
  if (shareY) {
    if (length(unique(yAxisN)) > 1) {
      warning("Must have a consistent number of axes per 'subplot' to share them.")
    } else {
      yAxisID <- rep(rep(seq_len(nrows * unique(xAxisN)), each = ncols, length.out = length(plots)), unique(yAxisN))
    }
  }
  # current "axis" names
  xCurrentNames <- unlist(lapply(xAxes, names))
  yCurrentNames <- unlist(lapply(yAxes, names))
  xNewNames <- paste0(
    sub("[0-9]+$", "", xCurrentNames), 
    sub("^1$", "", xAxisID)
  )
  yNewNames <- paste0(
    sub("[0-9]+$", "", yCurrentNames), 
    sub("^1$", "", yAxisID)
  )
  xAxisMap <- setNames(xCurrentNames, xNewNames)
  yAxisMap <- setNames(yCurrentNames, yNewNames)
  # split the map by plot ID
  xAxisMap <- split(xAxisMap, rep(seq_along(plots), xAxisN))
  yAxisMap <- split(yAxisMap, rep(seq_along(plots), yAxisN))
  # domains of each subplot
  domainInfo <- get_domains(
    length(plots), nrows, margin, widths = widths, heights = heights
  )
  for (i in seq_along(plots)) {
    # map axis object names
    xMap <- xAxisMap[[i]]
    yMap <- yAxisMap[[i]]
    xAxes[[i]] <- setNames(xAxes[[i]], names(xMap))
    yAxes[[i]] <- setNames(yAxes[[i]], names(yMap))
    # for cartesian, bump corresponding axis anchor
    for (j in seq_along(xAxes[[i]])) {
      if (grepl("^geo", names(xAxes[[i]][j]))) next
      map <- yMap[yMap %in% sub("y", "yaxis", xAxes[[i]][[j]]$anchor %||% "y")]
      xAxes[[i]][[j]]$anchor <- sub("axis", "", names(map))
    }
    for (j in seq_along(yAxes[[i]])) {
      if (grepl("^geo", names(yAxes[[i]][j]))) next
      map <- xMap[xMap %in% sub("x", "xaxis", yAxes[[i]][[j]]$anchor %||% "x")]
      yAxes[[i]][[j]]$anchor <- sub("axis", "", names(map))
    }
    # map trace xaxis/yaxis/geo attributes
    for (key in c("geo", "xaxis", "yaxis")) {
      oldAnchors <- unlist(lapply(traces[[i]], "[[", key))
      if (!length(oldAnchors)) next
      axisMap <- if (key == "yaxis") yMap else xMap
      axisMap <- setNames(sub("axis", "", axisMap), sub("axis", "", names(axisMap)))
      newAnchors <- names(axisMap)[match(oldAnchors, axisMap)]
      traces[[i]] <- Map(function(tr, a) { tr[[key]] <- a; tr }, traces[[i]], newAnchors)
    }
    # rescale domains according to the tabular layout
    xDom <- as.numeric(domainInfo[i, c("xstart", "xend")])
    yDom <- as.numeric(domainInfo[i, c("yend", "ystart")])
    reScale <- function(old, new) {
      sort(scales::rescale(
        old %||% c(0, 1), new, from = c(0, 1)
      ))
    }
    xAxes[[i]] <- lapply(xAxes[[i]], function(ax) {
      if (all(c("x", "y") %in% names(ax$domain))) {
        # geo domains are different from cartesian
        ax$domain$x <- reScale(ax$domain$x, xDom)
        ax$domain$y <- reScale(ax$domain$y, yDom)
      } else {
        ax$domain <- reScale(ax$domain, xDom)
      }
      ax
    })
    yAxes[[i]] <- lapply(yAxes[[i]], function(ax) {
      if (all(c("x", "y") %in% names(ax$domain))) {
        # geo domains are different from cartesian
        ax$domain$x <- reScale(ax$domain$x, xDom)
        ax$domain$y <- reScale(ax$domain$y, yDom)
      } else {
        ax$domain <- reScale(ax$domain, yDom)
      }
      ax
    })
  }
  # start merging the plots into a single subplot
  p <- list(
    data = Reduce(c, traces),
    layout = Reduce(modifyList, c(xAxes, rev(yAxes)))
  )
  # reposition shapes and annotations
  annotations <- Map(reposition, annotations, split(domainInfo, seq_along(plots)))
  shapes <- Map(reposition, shapes, split(domainInfo, seq_along(plots)))
  p$layout$annotations <- Reduce(c, annotations)
  p$layout$shapes <- Reduce(c, shapes)
  # merge non-axis layout stuff
  layouts <- lapply(layouts, function(x) x[!grepl("^[x-y]axis|^geo", names(x))] %||% list())
  if (which_layout != "merge") {
    if (!is.numeric(which_layout)) warning("which_layout must be numeric")
    if (!all(idx <- which_layout %in% seq_along(plots))) {
      warning("which_layout is referencing non-existant layouts")
      which_layout <- which_layout[idx]
    }
    layouts <- layouts[which_layout]
  }
  p$layout <- c(p$layout, Reduce(modifyList, layouts))
  
  res <- hash_plot(data.frame(), p)
  prefix_class(res, "plotly_subplot")
}


get_domains <- function(nplots = 1, nrows = 1, margins = 0.01, 
                        widths = NULL, heights = NULL) {
  if (length(margins) == 1) margins <- rep(margins, 4)
  if (length(margins) != 4) stop("margins must be length 1 or 4", call. = FALSE)
  ncols <- ceiling(nplots / nrows)
  widths <- widths %||% rep(1 / ncols, ncols)
  heights <- heights %||% rep(1 / nrows, nrows)
  if (length(widths) != ncols) {
    stop("The length of the widths argument must be equal ",
         "to the number of columns", call. = FALSE)
  }
  if (length(heights) != nrows) {
    stop("The length of the heights argument is ", length(heights),
         ", but the number of rows is ", nrows, call. = FALSE)
  }
  if (any(widths < 0) | any(heights < 0)) {
    stop("The widths and heights arguments must contain positive values")
  }
  if (sum(widths) > 1 | sum(heights) > 1) {
    stop("The sum of the widths and heights arguments must be less than 1")
  }
  
  widths <- cumsum(c(0, widths))
  heights <- cumsum(c(0, heights))
  # 'center' these values if there is still room left 
  widths <- widths + (1 - max(widths)) / 2
  heights <- heights + (1 - max(heights)) / 2
  
  xs <- vector("list", ncols)
  for (i in seq_len(ncols)) {
    xs[[i]] <- c(
      xstart = widths[i] + if (i == 1) 0 else margins[1],
      xend = widths[i + 1] - if (i == ncols) 0 else margins[2]
    )
  }
  xz <- rep_len(xs, nplots)
  
  ys <- vector("list", nrows)
  for (i in seq_len(nplots)) {
    j <- ceiling(i / ncols)
    ys[[i]] <- c(
      ystart = 1 - (heights[j]) - if (j == 1) 0 else margins[3],
      yend = 1 - (heights[j + 1]) + if (j == nrows) 0 else margins[4]
    )
  }
  list2df(Map(c, xz, ys))
}

list2df <- function(x, nms) {
  stopifnot(length(unique(sapply(x, length))) == 1)
  m <- if (length(x) == 1) t(x[[1]]) else Reduce(rbind, x)
  row.names(m) <- NULL
  df <- data.frame(m)
  if (!missing(nms)) setNames(df, nms) else df
}

# translate x/y positions according to domain objects 
# (useful mostly for repositioning annotations/shapes in subplots)
reposition <- function(obj, domains) {
  # we need x and y in order to rescale them!
  for (i in seq_along(obj)) {
    o <- obj[[i]]
    # TODO: this implementation currently assumes xref/yref == "paper"
    # should we support references to axis objects as well?
    for (j in c("x", "x0", "x1")) {
      if (is.numeric(o[[j]])) {
        obj[[i]][[j]] <- scales::rescale(
          o[[j]], as.numeric(domains[c("xstart", "xend")]), from = c(0, 1)
        )
      }
    }
    for (j in c("y", "y0", "y1")) {
      if (is.numeric(o[[j]])) {
        obj[[i]][[j]] <- scales::rescale(
          o[[j]], as.numeric(domains[c("yend", "ystart")]), from = c(0, 1)
        )
      }
    }
  }
  obj
}
