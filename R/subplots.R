#' View multiple plots in a single view
#' 
#' @param ... any number of plotly objects
#' @param nrows number of rows for laying out plots in a grid-like structure.
#' Only used if no domain is already specified.
#' @param widths relative width of each column on a 0-1 scale. By default all
#' columns have an equal relative width.
#' @param heights relative height of each row on a 0-1 scale. By default all
#' rows have an equal relative height.
#' @param share determines whether x/y/both axes are shared. 
#' @param which_layout adopt the layout of which plot? If the default value of 
#' "merge" is used, all plot level layout options will be included in the final 
#' layout. This argument also accepts a numeric vector which will restric
#' @param margin either a single value or four values (all between 0 and 1).
#' If four values are provided, the first is used as the left margin, the second
#' is used as the right margin, the third is used as the top margin, and the
#' fourth is used as the bottom margin.
#' If a single value is provided, it will be used as all four margins. 
#' @return A plotly object
#' @export
#' @author Carson Sievert
#' @examples \dontrun{
#' p1 <- plot_ly(economics, x = date, y = uempmed, showlegend = F)
#' p2 <- plot_ly(economics, x = date, y = unemploy, showlegend = F)
#' subplot(p1, p2, p1, p2, nrows = 2)
#' }

subplot <- function(..., nrows = 1, widths = NULL, heights = NULL, share = NULL, 
                    which_layout = "merge", margin = 0.02) {
  # build each plot and collect relevant info 
  plots <- lapply(list(...), plotly_build)
  traces <- lapply(plots, "[[", "data")
  layouts <- lapply(plots, "[[", "layout")
  shapes <- lapply(layouts, "[[", "shapes")
  # keep non axis title annotations
  annotations <- lapply(layouts, function(x) {
    axes <- vapply(x$annotations, function(a) identical(a$annotationType, "axis"), logical(1))
    x$annotations[!axes]
  })
  # collect axis objects
  xAxes <- lapply(layouts, function(x) {
    x[grepl("^xaxis", names(x))] %||% list(xaxis = list(domain = c(0, 1), anchor = "y"))
  })
  yAxes <- lapply(layouts, function(x) {
    x[grepl("^yaxis", names(x))] %||% list(yaxis = list(domain = c(0, 1), anchor = "x"))
  })
  # remove their titles
  xAxes <- lapply(xAxes, function(x) lapply(x, function(y) { y$title <- NULL; y }))
  yAxes <- lapply(yAxes, function(x) lapply(x, function(y) { y$title <- NULL; y }))
  # number of x/y axes per plot
  xAxisN <- vapply(xAxes, length, numeric(1))
  yAxisN <- vapply(yAxes, length, numeric(1))
  # old -> new axis name dictionary
  xAxisMap <- setNames(
    unlist(lapply(xAxes, names)),
    paste0("xaxis", sub("^1$", "", seq_len(sum(xAxisN))))
  )
  yAxisMap <- setNames(
    unlist(lapply(yAxes, names)),
    paste0("yaxis", sub("^1$", "", seq_len(sum(yAxisN))))
  )
  # split the map by plot ID
  xAxisMap <- split(xAxisMap, rep(seq_along(plots), xAxisN))
  yAxisMap <- split(yAxisMap, rep(seq_along(plots), yAxisN))
  # domains of each subplot
  # TODO: allow control of column width and row height!
  domainInfo <- get_domains(
    length(plots), nrows, margin, widths = widths, heights = heights
  )
  # reposition shapes and annotations
  annotations <- Map(reposition, annotations, split(domainInfo, seq_along(plots)))
  shapes <- Map(reposition, shapes, split(domainInfo, seq_along(plots)))
  # rename axis objects, anchors, and scale their domains
  for (i in seq_along(plots)) {
    xMap <- xAxisMap[[i]]
    yMap <- yAxisMap[[i]]
    xDom <- as.numeric(domainInfo[i, c("xstart", "xend")])
    yDom <- as.numeric(domainInfo[i, c("yend", "ystart")])
    for (j in seq_along(xAxes[[i]])) {
      # before bumping axis anchor, bump trace info, where appropriate
      traces[[i]] <- lapply(traces[[i]], function(tr) {
        tr$xaxis <- tr$xaxis %||% "x"
        tr$xaxis[sub("axis", "", xMap[[j]]) %in% tr$xaxis] <- sub("axis", "", names(xMap[j]))
        tr
      })
      # bump anchors
      map <- yMap[yMap %in% sub("y", "yaxis", xAxes[[i]][[j]]$anchor %||% "y")]
      xAxes[[i]][[j]]$anchor <- sub("axis", "", names(map))
      xAxes[[i]][[j]]$domain <- sort(scales::rescale(
        xAxes[[i]][[j]]$domain %||% c(0, 1), xDom, from = c(0, 1)
      ))
    }
    for (j in seq_along(yAxes[[i]])) {
      traces[[i]] <- lapply(traces[[i]], function(tr) {
        tr$yaxis <- tr$yaxis %||% "y"
        tr$yaxis[sub("axis", "", yMap[[j]]) %in% tr$yaxis] <- sub("axis", "", names(yMap[j]))
        tr
      })
      map <- xMap[xMap %in% sub("x", "xaxis", yAxes[[i]][[j]]$anchor %||% "x")]
      yAxes[[i]][[j]]$anchor <- sub("axis", "", names(map))
      yAxes[[i]][[j]]$domain <- sort(scales::rescale(
        yAxes[[i]][[j]]$domain %||% c(0, 1), yDom, from = c(0, 1)
      ))
    }
    xAxes[[i]] <- setNames(xAxes[[i]], names(xMap))
    yAxes[[i]] <- setNames(yAxes[[i]], names(yMap))
  }
  
  # start merging the plots into a single subplot
  p <- list(
    data = Reduce(c, traces),
    layout = Reduce(c, c(xAxes, yAxes))
  )
  p$layout$annotations <- Reduce(c, annotations)
  p$layout$shapes <- Reduce(c, shapes)
  
  # merge non-axis layout stuff
  layouts <- lapply(layouts, function(x) x[!grepl("^[x-y]axis", names(x))] %||% list())
  if (which_layout != "merge") {
    if (!is.numeric(which_layout)) warning("which_layout must be numeric")
    if (!all(idx <- which_layout %in% seq_along(plots))) {
      warning("which_layout is referencing non-existant layouts")
      which_layout <- which_layout[idx]
    }
    layouts <- layouts[which_layout]
  }
  p$layout <- c(p$layout, Reduce(modifyList, layouts))
  
  hash_plot(data.frame(), p)
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
    stop("The length of the heights argument must be equal ",
         "to the number of rows", call. = FALSE)
  }
  if (any(widths < 0 | heights < 0)) {
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
