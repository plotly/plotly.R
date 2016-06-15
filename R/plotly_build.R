#' 'Build' (i.e., evaluate) a plotly object
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
#' # the unevaluated plotly object
#' str(p)
#' # the evaluated data
#' str(plotly_build(p)$x$data)
#' 
plotly_build <- function(p) {
  UseMethod("plotly_build")
}

#' @export
plotly_build.default <- function(p) {
  p
}

#' @export
plotly_build.gg <- function(p) {
  ggplotly(p)
}

#' @export
plotly_build.plotly <- function(p) {
  
  layouts <- Map(function(x, y) {
    
    is_formula <- vapply(x, is.formula, logical(1))
    fl <- lazyeval::as_f_list(x[is_formula] %||% list())
    for (var in names(fl)) {
      x[[var]] <- lazyeval::f_eval(fl[[var]], plotly_data(p, y))
    }
    x[lengths(x) > 0]
    
  }, p$x$layoutAttrs, names(p$x$layoutAttrs))
  
  # get rid of the data -> layout mapping and merge all the layouts
  # into a single layout (more recent layouts will override older ones)
  p$x$layoutAttrs <- NULL
  p$x$layout <- modifyList(p$x$layout %||% list(), Reduce(modifyList, layouts) %||% list())
  
  dats <- Map(function(x, y) {
    
    is_formula <- vapply(x, is.formula, logical(1))
    fl <- lazyeval::as_f_list(x[is_formula] %||% list())
    d <- plotly_data(p, y)
    grps <- dplyr::groups(d)
    # insert missing values to differentiate groups
    if (length(grps) > 0) {
      d <- group2NA(d, groupNames = as.character(grps))
    }
    # perform the evaluation
    for (var in names(fl)) {
      x[[var]] <- lazyeval::f_eval(fl[[var]], d)
      varname <- sub("^~", "", deparse2(fl[[var]]))
      # deparse axis names and add to layout
      if (any(c("x", "y", "z") %in% var)) {
        if (is3d(x$type)) {
          p$x$layout$scene[[paste0(var, "axis")]]$title <<- varname
        } else {
          p$x$layout[[paste0(var, "axis")]]$title <<- varname
        }
      }
    }
    
    x$type <- verify_type(x$type)
    
    x[lengths(x) > 0]
    
  }, p$x$attrs, names(p$x$attrs))
  
  # "transforms" (i.e., apply scaling to) for special arguments
  # TODO: should non-formula object names populate titles?
  colorTitle <- unlist(lapply(p$x$attrs, "[[", "color"))[[1]]
  dats <- mapColor(dats, title = sub("^~", "", deparse2(colorTitle)))
  dats <- mapSymbol(dats)
  dats <- mapLinetype(dats)
  
  # traceify by the interaction of discrete variables
  traces <- list()
  for (i in seq_along(dats)) {
    d <- dats[[i]]
    params <- list(
      if (is.discrete(d[["color"]])) d[["color"]], 
      d[["symbol"]],
      d[["linetype"]]
    )
    params <- compact(params) %||% list(NULL)
    idx <- do.call("interaction", params)
    dats[[i]][["color"]] <- NULL
    dats[[i]][["symbol"]] <- NULL
    traces <- c(traces, traceify(dats[[i]], idx))
  }
  
  # it's possible that some things (like figures pulled from a plotly server)
  # already have "built" data
  p$x$data <- c(p$x$data, traces)
  
  # get rid of data -> vis mapping stuff
  p$x[c("visdat", "cur_data", "attrs")] <- NULL
  
  if (has_legend(p) && has_colorbar(p)) {
    # shrink the colorbar
    idx <- which(vapply(p$x$data, function(x) inherits(x, "plotly_colorbar"), logical(1)))
    p$x$data[[idx]]$marker$colorbar <- modifyList(
      p$x$data[[idx]]$marker$colorbar %||% list(), 
      list(len = 1/2, lenmode = "fraction", y = 1, yanchor = "top")
    )
    p$x$layout$legend <- modifyList(
      p$x$layout$legend %||% list(),
      list(y = 1/2, yanchor = "top")
    )
  }
  
  # traces can't have names
  p$x$data <- setNames(p$x$data, NULL)
  
  # verify plot attributes are legal according to the plotly.js spec
  # (and box attributes data_array attributes where appropriate)
  verify_plot(p)
}

# appends a new (empty) trace to generate (plot-wide) colorbar/colorscale
mapColor <- function(traces, title = "", na.color = "transparent") {
  color <- lapply(traces, "[[", "color")
  nColors <- lengths(color)
  # if no "top-level" color is present, return traces untouched
  if (all(nColors == 0)) {
    return(traces)
  }
  # color/colorscale/colorbar attribute placement depends on trace type and marker mode
  types <- vapply(traces, function(tr) tr$type, character(1))
  modes <- vapply(traces, function(tr) {
    tr$mode %||% if (any(lengths(tr) > 20)) "lines" else "markers+lines"
  }, character(1))
  hasMarker <- has_marker(types, modes)
  hasLine <- has_line(types, modes)
  hasText <- has_text(types, modes)
  hasZ <- !grepl("scatter", types) & 
    any(vapply(traces, function(tr) !is.null(tr$z), logical(1)))
  
  isNumeric <- vapply(color, is.numeric, logical(1))
  colors <- lapply(traces, "[[", "colors")
  
  if (any(isNumeric)) {
    palette <- compact(colors[isNumeric]) %||% viridisLite::viridis(10)
    if (is.list(palette) && length(palette) > 1) {
      stop("Multiple numeric color palettes specified (via the colors argument).\n",
           "When using the color/colors arguments, only one palette is allowed.",
           call. = FALSE)
    }
    # TODO: use ggstat::frange() when it's on CRAN?
    allColor <- unlist(color[isNumeric])
    rng <- range(allColor, na.rm = TRUE)
    colScale <- scales::col_numeric(palette, rng, na.color = na.color)
    # generate the colorscale to be shared across traces
    vals <- if (diff(rng) > 0) as.numeric(quantile(allColor)) else c(0, 1)
    colorScale <- matrix(c(scales::rescale(vals), colScale(vals)), ncol = 2)
    colorObj <- list(
      colorbar = list(title = as.character(title), ticklen = 2),
      cmin = rng[1],
      cmax = rng[2],
      colorscale = colorScale,
      showscale = FALSE
    )
    for (i in which(isNumeric)) {
      colorObj$color <- color[[i]]
      if (hasLine[[i]]) {
        traces[[i]]$line <- modifyList(traces[[i]]$line %||% list(), colorObj)
      }                 
      if (hasMarker[[i]]) {
        traces[[i]]$marker <- modifyList(traces[[i]]$marker %||% list(), colorObj)
      }
      if (hasZ[[i]]) {
        traces[[i]] <- modifyList(traces[[i]] %||% list(), colorObj)
      }
      if (hasText[[i]]) {
        warning("Numeric color variables cannot (yet) be mapped to text.\n",
                "Feel free to make a feature request \n", 
                "https://github.com/plotly/plotly.js", call. = FALSE)
      }
    }
    # add an "empty" trace with the colorbar
    colorObj$color <- rng
    colorObj$showscale <- TRUE
    colorBarTrace <- list(
      x = range(unlist(lapply(traces, "[[", "x")), na.rm = TRUE),
      y = range(unlist(lapply(traces, "[[", "y")), na.rm = TRUE),
      type = "scatter",
      mode = "markers",
      opacity = 0,
      hoverinfo = "none",
      showlegend = FALSE,
      marker = colorObj
    )
    traces[[length(traces) + 1]] <- structure(colorBarTrace, class = "plotly_colorbar")
  }
  
  if (any(!isNumeric)) {
    allColor <- unlist(color[!isNumeric])
    lvls <- unique(allColor)
    N <- length(lvls)
    palette <- compact(colors[!isNumeric]) %||% 
      if (is.ordered(allColor)) viridisLite::viridis(N) else RColorBrewer::brewer.pal(N, "Set2")
    if (is.list(palette) && length(palette) > 1) {
      stop("Multiple numeric color palettes specified (via the colors argument).\n",
           "When using the color/colors arguments, only one palette is allowed.",
           call. = FALSE)
    }
    colScale <- scales::col_factor(palette, levels = lvls, na.color = na.color)
    for (i in which(!isNumeric)) {
      if (hasLine[[i]]) {
        traces[[i]]$line$color <- colScale(color[[i]])
      }                 
      if (hasMarker[[i]]) {
        traces[[i]]$marker$color <- colScale(color[[i]])
      }
      if (hasText[[i]]) {
        traces[[i]]$textfont$color <- colScale(color[[i]])
      }
      
    }
  }
  
  traces
}

mapSymbol <- function(traces) {
  symbolList <- lapply(traces, "[[", "symbol")
  nSymbols <- lengths(symbolList)
  # if no "top-level" symbol is present, return traces untouched
  if (all(nSymbols == 0)) {
    return(traces)
  }
  symbol <- unlist(compact(symbolList))
  if (!is.null(symbol) && !is.discrete(symbol)) {
    warning("Coercing the symbol variable to a factor since\n", 
            "symbols only make sense for discrete variables", call. = FALSE)
    symbol <- as.factor(symbol)
  }
  N <- length(unique(symbol))
  if (N > 8) {
    warning("You've mapped a variable with ", N, " different levels to symbol.\n",
            "It's very difficult to perceive more than 8 different symbols\n",
            "in a single plot.")
  }
  # symbol values are duplicated (there is a valid numeric and character string for each symbol)
  validSymbols <- as.character(Schema$traces$scatter$attributes$marker$symbol$values)
  symbols <- unique(unlist(lapply(traces, "[[", "symbols"))) %||% 
    grep("[0-9]", validSymbols, invert = TRUE, value = TRUE)
  illegalSymbols <- setdiff(symbols, validSymbols)
  if (length(illegalSymbols)) {
    stop("The following are not valid symbol codes:\n",
         paste(illegalSymbols, collapse = ", "), 
         "Valid symbols include:\n'",
         paste(validSymbols, collapse = "', '"),
         call. = FALSE)
  }
  palette <- setNames(symbols[seq_len(N)], unique(symbol))
  for (i in which(nSymbols > 0)) {
    traces[[i]]$marker$symbol <- as.character(palette[symbolList[[i]]])
    # ensure the mode is set so that the symbol is relevant
    if (!grepl("markers", traces[[i]]$mode %||% "")) {
      message("Adding markers to mode; otherwise symbol would have no effect.")
      traces[[i]]$mode <- paste0(traces[[i]]$mode, "+markers")
    }
  }
  traces
}



mapLinetype <- function(traces) {
  linetypeList <- lapply(traces, "[[", "linetype")
  nLinetypes <- lengths(linetypeList)
  # if no "top-level" linetype is present, return traces untouched
  if (all(nLinetypes == 0)) {
    return(traces)
  }
  linetype <- unlist(compact(linetypeList))
  if (!is.null(linetype) && !is.discrete(linetype)) {
    warning("Coercing the linetype variable to a factor since\n", 
            "linetype only make sense for discrete variables", call. = FALSE)
    linetype <- as.factor(linetype)
  }
  N <- length(unique(linetype))
  validLinetypes <- as.character(
    Schema$traces$scatter$attributes$line$dash$values
  )
  if (N > length(validLinetypes)) {
    warning("linetype has ", N, " levels.\n", "plotly.js only has ", 
            length(validLinetypes), " different line types", call. = TRUE)
  }
  linetypes <- unique(unlist(lapply(traces, "[[", "linetypes"))) %||% validLinetypes
  illegalLinetypes <- setdiff(linetypes, validLinetypes)
  if (length(illegalLinetypes)) {
    stop("The following are not valid symbol codes:\n'",
         paste(illegalLinetypes, collapse = "', '"), 
         "Valid linetypes include:\n'",
         paste(validLinetypes, collapse = "', '"),
         call. = FALSE)
  }
  palette <- setNames(linetypes[seq_len(N)], unique(linetype))
  for (i in which(nLinetypes > 0)) {
    traces[[i]][["line"]][["dash"]] <- as.character(palette[linetypeList[[i]]])
    # ensure the mode is set so that the linetype is relevant
    if (!grepl("lines", traces[[i]]$mode %||% "")) {
      message("Adding lines to mode; otherwise linetype would have no effect.")
      traces[[i]]$mode <- paste0(traces[[i]]$mode, "+lines")
    }
    
  }
  traces
}


# break up a single trace into multiple traces according to values stored 
# a particular key name
traceify <- function(dat, x = NULL) {
  if (length(x) == 0) return(list(dat))
  # the order of lvls determines the order in which traces are drawn
  # for ordered factors at least, it makes sense to draw the highest level first
  # since that _should_ be the darkest color in a sequential pallette
  lvls <- if (is.factor(x)) rev(levels(x)) else unique(x)
  n <- length(x)
  # recursively search for a non-list of appropriate length (if it is, subset it)
  recurse <- function(z, n, idx) {
    if (is.list(z)) lapply(z, recurse, n, idx) else if (length(z) == n) uniq(z[idx]) else z
  }
  new_dat <- list()
  for (j in seq_along(lvls)) {
    new_dat[[j]] <- lapply(dat, function(y) recurse(y, n, x %in% lvls[j]))
    new_dat[[j]]$name <- lvls[j]
  }
  return(new_dat)
}
