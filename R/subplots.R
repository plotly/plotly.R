#' View multiple plots in a single view
#' 
#' @param ... any number of plotly objects
#' @param nrows number of rows for laying out plots in a grid-like structure.
#' Only used if no domain is already specified.
#' @param which_layout adopt the layout of which plot?
#' @return A plotly object
#' @export
#' @author Carson Sievert
#' @examples \dontrun{
#' p1 <- plot_ly(economics, x = date, y = uempmed, showlegend = F)
#' p2 <- plot_ly(economics, x = date, y = unemploy, showlegend = F)
#' offline(subplot(p1, p2, p1, p2, nrows = 2))
#' }

subplot <- function(..., nrows = 1, which_layout = 1) {
  # note that dots is a _list of plotlys_
  dots <- lapply(list(...), plotly_build)
  # put existing plot anchors and domain information into a tidy format
  # (geo, xaxis, or yaxis can be used to anchor traces on different plots)
  p_info <- list()
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
      p_info[[i * j]] <- c(info, plot = i, trace = j)
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
  p_info$xaxis <- sub("x1", "x", paste0("x", p_info$key))
  p_info$yaxis <- sub("y1", "y", paste0("y", p_info$key))
  
  # Only do domain computations if they are _completely_ missing
  # (I don't think it makes sense to support partial specification of domains)
  if (all(is.na(with(p_info, c(xstart, xend, ystart, yend))))) {
    nplots <- max(p_info$key)
    ncols <- floor(nplots / nrows)
    xdom <- get_domains(nplots, ncols)
    ydom <- get_domains(nplots, nrows)
    xdf <- cbind(
      list2df(xdom, c("xstart", "xend")),
      key = seq_len(nplots)
    )
    ydf <- list2df(ydom, c("ystart", "yend"))
    # make sure the first plot appears in the top row
    ydf <- ydf[order(ydf$ystart, decreasing = TRUE), ]
    ydf$key <- seq_len(nplots)
    # overwrite relevant info 
    p_info <- p_info[!grepl("start$|end$", names(p_info))]
    p_info <- plyr::join(p_info, xdf, by = "key")
    p_info <- plyr::join(p_info, ydf, by = "key")
  }
  
  # empty plot container that we'll fill up with new info
  p <- list(
    data = vector("list", nrow(p_info)),
    layout = dots[[which_layout]]$layout
  )
  p_info$plot <- as.numeric(p_info$plot)
  p_info$trace <- as.numeric(p_info$trace)
  for (i in seq_along(p$data)) {
    info <- p_info[i, ]
    xdom <- c(info$xstart, info$xend)
    ydom <- c(info$ystart, info$yend)
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
      p$layout[[xaxis]] <- modifyList(
        p$layout[[xaxis]] %||% list(),
        list(domain = xdom, anchor = info$yaxis)
      )
      p$layout[[yaxis]] <- modifyList(
        p$layout[[yaxis]] %||% list(),
        list(domain = ydom, anchor = info$xaxis)
      )
      p$data[[i]]$xaxis <- info$xaxis
      p$data[[i]]$yaxis <- info$yaxis
    }
  }
  hash_plot(data.frame(), p)
}


# margins should shrink as # of plots increase
get_domains <- function(nplots = 1, nsplits = 1, mar = 0.1 / nsplits, 
                        decreasing = FALSE) {
  if (nsplits == 1) {
    lapply(vector("list", nplots), function(x) c(0, 1))
  } else {
    domains <- vector("list", nsplits)
    for (i in seq_len(nsplits)) {
      l <-  ((i - 1) / nsplits) + ifelse(i == 1, 0, mar)
      u <- (i / nsplits) - ifelse(i == nsplits, 0, mar)
      domains[[i]] <- c(l, u)
    }
    rep_len(domains, nplots)
  }
}


list2df <- function(x, nms) {
  stopifnot(length(unique(sapply(x, length))) == 1)
  m <- Reduce(rbind, x)
  row.names(m) <- NULL
  df <- data.frame(m)
  if (!missing(nms)) setNames(df, nms) else df
}
