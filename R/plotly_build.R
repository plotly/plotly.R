#' 'Build' (i.e., evaluate) a plotly object
#' 
#' 
#' This generic function creates the list object sent to plotly.js
#' for rendering. Using this function can be useful for overriding defaults
#' provided by \code{ggplotly}/\code{plot_ly} or for debugging rendering
#' errors.
#' 
#' @param p a ggplot object, or a plotly object, or a list.
#' @export
#' @examples
#' 
#' p <- plot_ly(economics, x = ~date, y = ~pce)
#' # a plotly visualization object
#' str(p$x$data)
#' # the actual list of options sent to plotly.js
#' str(plotly_build(p))
#' 
#' p <- qplot(data = mtcars, wt, mpg, geom = c("point", "smooth"))
#' l <- plotly_build(p)
#' # turn off hoverinfo for the smooth (but keep it for the points)
#' l$x$data[[2]]$hoverinfo <- "none"
#' l$x$data[[3]]$hoverinfo <- "none"
#' l
#' 
plotly_build <- function(p) {
  UseMethod("plotly_build")
}

#' @export
plotly_build.gg <- function(p) {
  ggplotly(p)
}

#' @export
plotly_build.plotly <- function(p) {
  
  # Initiating a plot with plot_ly() should have a _list_ of layouts
  if (is.list(p$x$layout) && is.null(names(p$x$layout))) {
    p$x$layout <- lapply(p$x$layout, function(lay) {
      is_formula <- vapply(lay$attrs, is.formula, logical(1))
      fl <- lazyeval::as_f_list(lay$attrs[is_formula] %||% list())
      for (var in names(fl)) {
        lay[[var]] <- lazyeval::f_eval(fl[[var]], lay$rdata)
      }
      # tack on attributes, that aren't formulas, without evaluating them
      lay <- c(lay, lay$attrs[!is_formula])
      lay[c("attrs", "rdata")] <- NULL
      lay[lengths(lay) > 0]
    })
    # combine all the layouts into one layout 
    # (with later layout() calls overriding previous ones)
    p$x$layout <- Reduce(modifyList, p$x$layout)
  }
  
  p$x$data <- lapply(p$x$data, function(tr) {
    is_formula <- vapply(tr$attrs, is.formula, logical(1))
    fl <- lazyeval::as_f_list(tr$attrs[is_formula] %||% list())
    for (var in names(fl)) {
      tr[[var]] <- lazyeval::f_eval(fl[[var]], tr$rdata)
      varname <- sub("^~", "", deparse2(fl[[var]]))
      # deparse axis names and add to layout
      if (any(c("x", "y", "z") %in% var)) {
        if (is3d(tr$type)) {
          p$x$layout$scene[[paste0(var, "axis")]]$title <<- varname
        } else {
          p$x$layout[[paste0(var, "axis")]]$title <<- varname
        }
      }
    }
    # tack on attributes, that aren't formulas, without evaluating them
    tr <- c(tr, tr$attrs[!is_formula])
    tr[c("attrs", "rdata")] <- NULL
    tr[lengths(tr) > 0]
  })
  
  p
}

# returns a _list of traces_.
colorize <- function(dat, title = "") {
  cols <- dat[["color"]] %||% dat[["z"]]
  if (is.numeric(cols)) {
    # by default, use viridis::viridis(10) -> http://rud.is/b/2015/07/20/using-the-new-viridis-colormap-in-r-thanks-to-simon-garnier/
    colors <- dat[["colors"]] %||% viridis::viridis(10)
    cols <- as.vector(cols)
    rng <- range(cols, na.rm = TRUE)
    x <- seq(min(rng), max(rng), length.out = 10)
    colz <- scales::col_numeric(colors, rng, na.color = "transparent")(x)
    df <- if (length(cols) > 1) data.frame(scales::rescale(x), colz) 
    else data.frame(c(0, 1), rep(colz, 2))
    col_list <- list(
      colorbar = list(title = as.character(title)),
      colorscale = setNames(df, NULL)
    )
    # scatter-like traces can have both line and marker objects
    if (grepl("scatter", dat[["type"]] %||% "scatter")) {
      col_list$color <- cols
      dat[["marker"]] <- modifyList(col_list, dat[["marker"]] %||% list())
      #mode <- dat[["mode"]] %||% "markers+lines"
      # can't have a colorscale for both markers and lines???
      #dat[["line"]] <- modifyList(col_list, dat[["line"]] %||% list())
    } else {
      dat <- c(dat, col_list)
    }
    dat <- list(dat)
  } else { # discrete color scale
    lvls <- unique(cols)
    N <- length(lvls)
    default <- if (is.ordered(cols)) viridis::viridis(N) 
    else RColorBrewer::brewer.pal(N, "Set2")
    colors <- dat[["colors"]] %||% default
    colz <- scales::col_factor(colors, levels = lvls, na.color = "transparent")(lvls)
    dat <- traceify(dat, "color")
    dat <- Map(function(x, y) { x[["marker"]] <- c(x[["marker"]], list(color = y)); x }, 
               dat, colz)
  }
  dat <- lapply(dat, function(x) { x$color <- NULL; x$colors <- NULL; x })
  dat
}

symbolize <- function(dat) {
  # symbols really only make sense when markers are in the mode, right?
  dat$mode <- dat$mode %||% "markers"
  dat <- traceify(dat, "symbol")
  dat <- lapply(dat, function(x) { x$symbol <- NULL; x })
  N <- length(dat)
  if (N > 8) warning("Plotly supports 8 different symbols, but you have ", N, " levels!")
  symbols <- c('dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up')
  sym <- dat[[1]][["symbols"]][seq_len(N)] %||% symbols[seq_len(N)]
  dat <- Map(function(x, y) { x$marker$symbol <- y; x }, dat, sym)
  dat
}

# break up a single trace into multiple traces according to values stored 
# a particular key name
traceify <- function(dat, nm = "group") {
  x <- dat[[nm]]
  if (is.null(x)) {
    return(list(dat))
  } else {
    # the order of lvls determines the order in which traces are drawn
    # for ordered factors at least, it makes sense to draw the highest level first
    # since that _should_ be the darkest color in a sequential pallette
    lvls <- if (is.factor(x)) rev(levels(x)) else unique(x)
    n <- length(x)
    # recursively search for a non-list of appropriate length (if it is, subset it)
    recurse <- function(z, n, idx) {
      if (is.list(z)) lapply(z, recurse, n, idx) else if (length(z) == n) z[idx] else z
    }
    new_dat <- list()
    for (j in seq_along(lvls)) {
      new_dat[[j]] <- lapply(dat, function(y) recurse(y, n, x %in% lvls[j]))
      new_dat[[j]]$name <- lvls[j]
    }
    return(new_dat)
  }
}
