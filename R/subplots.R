#' View multiple plots in a single view
#' 
#' @param ... any number of plotly objects
#' @param nrows number of rows for laying out plots in a grid-like structure.
#' Only used if no domain is already specified.
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

subplot <- function(..., nrows = 1, which_layout = "merge", margin = 0.02) {
  # build each plot
  plots <- lapply(list(...), plotly_build)
  # rename axes, respecting the fact that each plot could be a subplot itself
  traces <- lapply(plots, "[[", "data")
  layouts <- lapply(plots, "[[", "layout")
  
  annotations <- compact(lapply(layouts, "[[", "annotations"))
  shapes <- compact(lapply(layouts, "[[", "shapes"))
  xAxes <- lapply(layouts, function(x) {
    x[grepl("^xaxis", names(x))] %||% 
      list(xaxis = list(domain = c(0, 1), anchor = "y"))
  })
  yAxes <- lapply(layouts, function(x) {
    x[grepl("^yaxis", names(x))] %||% 
      list(yaxis = list(domain = c(0, 1), anchor = "x"))
  })
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
  # get the domain of each "viewport"
  # TODO: allow control of column width and row height!
  domainInfo <- get_domains(length(plots), nrows, margin)
  for (i in seq_along(plots)) {
    xMap <- xAxisMap[[i]]
    yMap <- yAxisMap[[i]]
    xDom <- as.numeric(domainInfo[i, c("xstart", "xend")])
    yDom <- as.numeric(domainInfo[i, c("yend", "ystart")])
    for (j in seq_along(xAxes[[i]])) {
      # before bumping axis anchor, bump trace info, where appropriate
      traces[[i]] <- lapply(traces[[i]], function(tr) {
        tr$xaxis[tr$xaxis %in% sub("axis", "", xMap[[j]])] <- sub("axis", "", names(xMap[j]))
        tr
      })
      # bump anchors
      map <- yMap[yMap %in% sub("y", "yaxis", xAxes[[i]][[j]]$anchor)]
      xAxes[[i]][[j]]$anchor <- sub("axis", "", names(map))
      browser()
      xAxes[[i]][[j]]$domain <- sort(scales::rescale(
        xAxes[[i]][[j]]$domain, xDom, from = c(0, 1)
      ))
    }
    for (j in seq_along(yAxes[[i]])) {
      traces[[i]] <- lapply(traces[[i]], function(tr) {
        tr$yaxis[tr$yaxis == sub("axis", "", yMap[[j]])] <- sub("axis", "", names(yMap[j]))
        tr
      })
      map <- xMap[xMap %in% sub("x", "xaxis", yAxes[[i]][[j]]$anchor)]
      yAxes[[i]][[j]]$anchor <- sub("axis", "", names(map))
      yAxes[[i]][[j]]$domain <- sort(scales::rescale(
        yAxes[[i]][[j]]$domain, yDom, from = c(0, 1)
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
  # TODO: scale shape/annotation coordinates and incorporate them! 
  # Should we throw warning if [x-y]ref != "paper"?
  
  # merge non-axis layout stuff
  layouts <- lapply(layouts, function(x) x[!grepl("^[x-y]axis", names(x))])
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


get_domains <- function(nplots = 1, nrows = 1, margins = 0.01) {
  if (length(margins) == 1) margins <- rep(margins, 4)
  if (length(margins) != 4) stop("margins must be length 1 or 4", call. = FALSE)
  ncols <- ceiling(nplots / nrows)
  
  xs <- vector("list", ncols)
  for (i in seq_len(ncols)) {
    xs[[i]] <- c(
      xstart = ((i - 1) / ncols) + ifelse(i == 1, 0, margins[1]),
      xend = (i / ncols) - ifelse(i == ncols, 0, margins[2])
    )
  }
  xz <- rep_len(xs, nplots)
  
  ys <- vector("list", nrows)
  for (i in seq_len(nplots)) {
    j <- ceiling(i / ncols)
    ys[[i]] <- c(
      ystart = 1 - ((j - 1) / nrows) - ifelse(j == 1, 0, margins[3]),
      yend = 1 - (j / nrows) + ifelse(j == nrows, 0, margins[4])
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
