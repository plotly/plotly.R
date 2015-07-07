#' View multiple plots in a single view
#' 
#' @param ... any number of plotly objects
#' @param nrows number of rows for laying out plots in a grid-like structure.
#' Only used if no domain is already specified.
#' @param which_layout adopt the layout of which plot?
#' @return A plotly object
#' @export
#' @author Carson Sievert
#' 

# TODO: throw warning if more than one _unique_ axis is predefined in any plot?
subplot <- function(..., nrows = 1, which_layout = 1) {
  dots <- list(...)
  is_plotly <- vapply(dots, is.plotly, logical(1), USE.NAMES = FALSE)
  if (!all(is_plotly)) {
    warning("Every argument to this function should be plotly object.",
            "I detected non-plotly objects in the following arguments",
            paste(which(!is_plotly), collapse = ", "), 
            "These arguments will be ignored")
    dots <- dots[is_plotly]
  }
  if (length(dots) == 1) return(dots)
  dots <- lapply(dots, function(x) plotly_build(get_plot(x)))
  # make sure each 'subplot' has each unique set of axes
  dat <- lapply(dots, "[[", "data")
  # for a particular plot, get axis identifiers
  ids <- function(x, axis = "x") {
    axes <- unlist(lapply(x, "[[", paste0(axis, "axis")))
    as.integer(sub(paste0("^", axis), "", axes))
  }
  xaxes <- lapply(dat, ids)
  yaxes <- lapply(dat, ids, "y")
  # if xaxis/yaxis info doesn't exist, assume there is one axis in each plot
  xaxes[sapply(xaxes, length) == 0] <- 1
  yaxes[sapply(yaxes, length) == 0] <- 1
  # bump axes accordingly
  xs <- cumsum(lapply(xaxes, max)) - 1
  ys <- cumsum(lapply(xaxes, max)) - 1
  for (i in seq(2, length(dat))) { # plot level
    for (j in seq_along(dat[[i]])) { # trace level
      dat[[i]][[j]]$xaxis <- paste0("x", xs[i] + xaxes[[i]][j])
      dat[[i]][[j]]$yaxis <- paste0("y", ys[i] + yaxes[[i]][j])
    }
  }
  for (i in seq_along(dat)) dat[[i]] <- dat[[i]][[1]]
  # now, figure out the domain spacing
  ls <- lapply(dots, "[[", "layout")
  nplots <- sum(is_plotly)
  xdom <- lapply(ls, function(x) x$xaxis$domain)
  ydom <- lapply(ls, function(x) x$yaxis$domain)
  is_null_x <- vapply(xdom, is.null, logical(1), USE.NAMES = FALSE)
  is_null_y <- vapply(ydom, is.null, logical(1), USE.NAMES = FALSE)
  # if no domain is specified, use the nrows args; otherwise, trust users domain?
  if (all(is_null_x) && all(is_null_y)) {
    ncols <- floor(nplots / nrows)
    xdom <- get_domains(nplots, ncols)
    ydom <- get_domains(nplots, nrows)
  }
  xaxes <- mapply(function(x, y) list(domain = x, anchor = y),
                  xdom, paste0("y", seq_len(nplots)), SIMPLIFY = FALSE)
  xaxes <- setNames(xaxes, sub("1", "", paste0("xaxis", seq_len(nplots))))
  yaxes <- mapply(function(x, y) list(domain = x, anchor = y),
                  ydom, paste0("x", seq_len(nplots)), SIMPLIFY = FALSE)
  yaxes <- setNames(yaxes, sub("1", "", paste0("yaxis", seq_len(nplots))))
  layout <- dots[[which_layout]]$layout
  if (is.null(layout)) layout <- list()
  layout <- modifyList(layout, xaxes)
  layout <- modifyList(layout, yaxes)
  hash_plot(data.frame(), list(data = dat, layout = layout))
}


# margins should shrink as # of plots increase
get_domains <- function(nplots = 1, nsplits = 1, mar = 0.1 / nsplits) {
  if (nsplits == 1) {
    lapply(vector("list", nplots), function(x) c(0, 1))
  } else {
    domains <- vector("list", nsplits)
    for (i in seq_len(nsplits)) {
      l <- ((i - 1) / nsplits) + ifelse(i == 1, 0, mar)
      u <- (i / nsplits) - ifelse(i == nsplits, 0, mar)
      domains[[i]]  <- c(l, u)
    }
    rep_len(domains, nplots)
  }
}
