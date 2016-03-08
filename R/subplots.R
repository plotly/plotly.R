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
  # note that dots is a _list of plotlys_
  dots <- lapply(list(...), plotly_build)
  # put existing plot anchors and domain information into a tidy format
  # (geo, xaxis, or yaxis can be used to anchor traces on different plots)
  p_info <- list()
  ctr <- 1
  for (i in seq_along(dots)) {
    dat <- dots[[i]]$data
    layout <- dots[[i]]$layout
    for (j in seq_along(dat)) {
      tr <- dat[[j]]
      idx <- if (j == 1) "" else j
      geo <- unique(tr$geo) %||% ""
      # if a valid geo property exists, use that and ignore x/y axis properties
      info <- if (grepl("^geo[0-9]+$", geo)) {
        d <- layout[[paste0("geo", idx)]][["domain"]] %||% list(x = NA, y = NA)
        c(
          geo = sub("^geo1$", "geo", geo),
          xaxis = "",
          xstart = d$x[1],
          xend = d$x[2],
          yaxis = "",
          ystart = d$y[1],
          yend = d$y[2]
        )
      } else {
        dx <- layout[[paste0("xaxis", idx)]][["domain"]] %||% NA
        dy <- layout[[paste0("yaxis", idx)]][["domain"]] %||% NA
        c(
          geo = "",
          xaxis = unique(tr$xaxis) %||% "",
          xstart = dx[1],
          xend = dx[2],
          yaxis = unique(tr$yaxis) %||% "",
          ystart = dy[1],
          yend = dy[2]
        )
      }
      p_info[[ctr]] <- c(info, plot = i, trace = j)
      ctr <- ctr + 1
    }
  }
  # put p_info into a data.frame()
  p_info <- Reduce(rbind, p_info)
  row.names(p_info) <- NULL
  p_info <- data.frame(p_info, stringsAsFactors = FALSE)
  # obtain the _actual_ plot id
  key <- with(p_info, paste0(geo, xaxis, yaxis, plot))
  p_info$key <- match(key, unique(key)) 
  # bump x/y axis anchors appropriately
  p_info$xaxis <- sub("^x1$", "x", paste0("x", p_info$key))
  p_info$yaxis <- sub("^y1$", "y", paste0("y", p_info$key))
  # Only do domain computations if they are _completely_ missing
  # (I don't think it makes sense to support partial specification of domains)
  if (all(is.na(with(p_info, c(xstart, xend, ystart, yend))))) {
    doms <- get_domains(max(p_info$key), nrows, margin)
    doms$key <- as.character(seq_len(nrow(doms)))
    p_info <- p_info[!names(p_info) %in% c("xstart", "xend", "ystart", "yend")]
    p_info <- merge(p_info, doms, by = "key", sort = FALSE)
  }
  # empty plot container that we'll fill up with new info
  p <- list(
    data = vector("list", nrow(p_info))
  )
  # merge layouts of the subplots
  ls <- if (which_layout == "merge") {
    lapply(dots, "[[", "layout")
  } else {
    if (!is.numeric(which_layout)) warning("which_layout must be numeric")
    if (!all(idx <- which_layout %in% seq_along(dots))) {
      warning("which_layout is referencing non-existant layouts")
      which_layout <- which_layout[idx]
    }
    lapply(dots[which_layout], "[[", "layout")
  }
  ls <- ls[!vapply(ls, is.null, logical(1))]
  p[["layout"]] <- Reduce(modifyList, ls)
  
  # tack on trace, domain, and anchor information
  p_info$plot <- as.numeric(p_info$plot)
  p_info$trace <- as.numeric(p_info$trace)
  for (i in seq_along(p$data)) {
    info <- p_info[i, ]
    xdom <- sort(c(info$xstart, info$xend))
    ydom <- sort(c(info$ystart, info$yend))
    p$data[[i]] <- dots[[info$plot]]$data[[info$trace]]
    if (grepl("^geo", info$geo)) {
      # carry over first geo object if this one is missing
      p$layout[[info$geo]] <- p$layout[[info$geo]] %||% p$layout[["geo"]]
      # add domains to the layout
      p$layout[[info$geo]] <- modifyList(
        p$layout[[info$geo]] %||% list(),
        list(domain = list(x = xdom, y = ydom))
      )
      # ensure the geo anchor is a single value
      p$data[[i]]$geo <- info$geo
    } else {
      xaxis <- sub("x", "xaxis", info$xaxis)
      yaxis <- sub("y", "yaxis", info$yaxis)
      # does this plot contain x/y axis styling? If so, use it 
      # (but overwrite domain/anchor info)
      l <- dots[[info$plot]]$layout
      p$layout[[xaxis]] <- modifyList(
        if (any(idx <- names(l) %in% "xaxis")) l[idx][[1]] else list(),
        list(domain = xdom, anchor = info$yaxis)
      )
      p$layout[[yaxis]] <- modifyList(
        if (any(idx <- names(l) %in% "yaxis")) l[idx][[1]] else list(),
        list(domain = ydom, anchor = info$xaxis)
      )
      p$data[[i]]$xaxis <- info$xaxis
      p$data[[i]]$yaxis <- info$yaxis
    }
  }
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
