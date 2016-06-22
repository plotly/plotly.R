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
plotly_build.list <- function(p) {
  as_widget(p)
}

#' @export
plotly_build.gg <- function(p) {
  ggplotly(p)
}

#' @export
plotly_build.plotly <- function(p) {
  
  layouts <- Map(function(x, y) {
    
    d <- plotly_data(p, y)
    x <- rapply(x, eval_attr, data = d, how = "list")
    x[lengths(x) > 0]
    
  }, p$x$layoutAttrs, names2(p$x$layoutAttrs))
  
  # get rid of the data -> layout mapping and merge all the layouts
  # into a single layout (more recent layouts will override older ones)
  p$x$layoutAttrs <- NULL
  p$x$layout <- modify_list(p$x$layout, Reduce(modify_list, layouts))
  
  # If type was not specified in plot_ly(), it doesn't create a trace unless
  # there are no other traces
  if (length(p$x$attrs) > 1 && is.null(p$x$attrs[[1]][["type"]])) {
    p$x$attrs[[1]] <- NULL
  }
  
  dats <- Map(function(x, y) {
    
    # add sensible axis names to layout
    for (i in c("x", "y", "z")) {
      nm <- paste0(i, "axis")
      idx <- which(names(x) %in% i)
      if (length(idx) == 1) {
        title <- deparse2(x[[idx]])
        if (is3d(x$type) || i == "z") {
          p$x$layout$scene[[nm]]$title <<- p$x$layout$scene[[nm]]$title %||% title
        } else {
          p$x$layout[[nm]]$title <<- p$x$layout[[nm]]$title %||% title
        }
      }
    }
    
    # perform the evaluation
    # TODO: remove/warn about missing values here? Provide a way to control na.color?
    dat <- plotly_data(p, y)
    x <- structure(rapply(x, eval_attr, data = dat, how = "list"), class = class(x))
    # ensure we have a trace type (depends on the # of data points)
    x <- verify_type(x)
    
    # TODO: if we ever provide some semantics for statistics, it should probably go here
    
    # gather the "built" or "evaluated" data
    nobs <- NROW(dat)
    isVar <- vapply(x, function(attr) length(attr) == nobs, logical(1))
    builtData <- data.frame(x[isVar & !names(x) %in% c("colors", "symbols", "linetypes")])
    builtData <- train_data(builtData, x)
    # TODO: provide a better way to clean up "high-level" attrs
    x[c("ymin", "ymax", "yend", "xend")] <- NULL
    x$.plotlyVariableMapping <- names(builtData)
    
    # find any groupings, so we can arrange the data now, 
    # and insert missing values (to create gaps between traces) later
    grps <- as.character(dplyr::groups(builtData))
    # does grouping even make sense for this trace type?
    hasGrp <- inherits(x, c("plotly_segment", "plotly_line", "plotly_polygon")) ||
      (grepl("scatter", x$type) && grepl("lines", x$mode %||% "lines"))
    if (length(grps) && hasGrp) {
      if (isTRUE(x$connectgaps)) {
        stop(
          "Can't set connectgaps=T in a trace that has 1 or more groups.", 
          call. = FALSE
        )
      }
      builtData$.plotlyGroupIndex <- interaction(builtData[, grps, drop = FALSE])
    }
    
    # build the index used to transform one "trace" into multiple traces
    discreteData <- builtData[vapply(builtData, is.discrete, logical(1))]
    discreteData <- discreteData[names(discreteData) %in% c("symbol", "linetype", "color")]
    if (NCOL(discreteData) > 0) {
      paste2 <- function(x, y) paste(x, y, sep = "<br>")
      builtData$.plotlyTraceIndex <- Reduce(paste2, discreteData)
    }
    # arrange the built data
    arrangeVars <- c(
      ".plotlyTraceIndex", "group", if (inherits(x, "plotly_line")) "x"
    )
    arrangeVars <- arrangeVars[arrangeVars %in% names(builtData)]
    if (length(arrangeVars)) {
      builtData <- dplyr::arrange_(builtData, arrangeVars)
    }
    # copy over to the trace data
    for (i in names(builtData)) {
      x[[i]] <- builtData[[i]]
    }
    
    # if appropriate, set the mode now since we need to reference it later
    attrLengths <- lengths(x)
    if (grepl("scatter", x$type) && is.null(x$mode)) {
      x$mode <- if (any(attrLengths > 20)) "lines" else "markers+lines"
    }
    
    x[attrLengths > 0]
    
  }, p$x$attrs, names2(p$x$attrs))
  
  # traceify by the interaction of discrete variables
  traces <- list()
  for (i in seq_along(dats)) {
    d <- dats[[i]]
    scaleAttrs <- names(d) %in% c("colors", "linetypes", "symbols", "sizes")
    traces <- c(traces, traceify(d[!scaleAttrs], d$.plotlyTraceIndex))
    if (i == 1) traces[[1]] <- c(traces[[1]], d[scaleAttrs])
  }
  
  # insert NAs to differentiate groups
  traces <- lapply(traces, function(x) {
    d <- data.frame(x[names(x) %in% c(x$.plotlyVariableMapping)])
    d <- group2NA(d, ".plotlyGroupIndex", retrace.first = inherits(x, "plotly_polygon"))
    for (i in x$.plotlyVariableMapping) {
      x[[i]] <- uniq(d[[i]])
    }
    x
  })
  
  # "transforms" of (i.e., apply scaling to) special arguments
  # IMPORTANT: scales are applied at the plot-level!!
  colorTitle <- unlist(lapply(p$x$attrs, function(x) {
    deparse2(x[["color"]] %||% x[["z"]])
  }))
  traces <- map_color(traces, title = paste(colorTitle, collapse = "<br>"))
  traces <- map_size(traces)
  traces <- map_symbol(traces)
  traces <- map_linetype(traces)
  
  # remove special mapping attributes
  for (i in seq_along(traces)) {
    mappingAttrs <- c(
      "color", "colors", "symbol", "symbols", 
      "linetype", "linetypes", "size", "sizes", 
      ".plotlyGroupIndex", ".plotlyTraceIndex", ".plotlyVariableMapping"
    )
    for (j in mappingAttrs) {
      traces[[i]][[j]] <- NULL
    }
  }
  
  # it's possible that the plot object already has some traces 
  # (like figures pulled from a plotly server)
  p$x$data <- setNames(c(p$x$data, traces), NULL)
  
  # get rid of data -> vis mapping stuff
  p$x[c("visdat", "cur_data", "attrs")] <- NULL
  
  if (has_colorbar(p) && has_legend(p)) {
    if (length(p$x$data) <= 2) {
      p$x$layout$showlegend <- FALSE
    } else {
      # shrink the colorbar
      idx <- which(vapply(p$x$data, function(x) inherits(x, "plotly_colorbar"), logical(1)))
      p$x$data[[idx]]$marker$colorbar <- modify_list(
        list(len = 1/2, lenmode = "fraction", y = 1, yanchor = "top"),
        p$x$data[[idx]]$marker$colorbar
      )
      p$x$layout$legend <- modify_list(
        list(y = 1/2, yanchor = "top"),
        p$x$layout$legend
      )
    }
  }
  
  # verify plot attributes are legal according to the plotly.js spec
  p <- verify_attr_names(p)
  # box up 'data_array' attributes where appropriate
  p <- verify_boxed(p)
  # if it makes sense, add markers/lines/text to mode
  p <- verify_mode(p)
  # annotations & shapes must be an array of objects
  # TODO: should we add anything else to this?
  p <- verify_arrays(p)
  # set a sensible hovermode if it hasn't been specified already
  p <- verify_hovermode(p)
  # try to convert to webgl if toWebGl was used
  verify_webgl(p)
}

# ----------------------------------------------------------------
# Functions used solely within plotly_build
# ----------------------------------------------------------------

train_data <- function(data, trace) {
  if (inherits(trace, "plotly_area")) {
    data$ymin <- 0
  }
  if (inherits(trace, "plotly_ribbon")) {
    data <- ribbon_dat(data)
  }
  if (inherits(trace, "plotly_segment")) {
    # TODO: this could be faster, more efficient
    data$.plotlyGroupIndex <- seq_len(NROW(data))
    data <- gather_(
      gather_(data, "tmp", "x", c("x", "xend")), 
      "tmp", "y", c("y", "yend")
    )
    data <- dplyr::arrange_(data[!names(data) %in% "tmp"], ".plotlyGroupIndex")
    data <- dplyr::distinct(data)
    data <- dplyr::group_by_(data, ".plotlyGroupIndex", add = TRUE)
  }
  # TODO: a lot more geoms!!!
  data
}


map_size <- function(traces) {
  sizeList <- lapply(traces, "[[", "size")
  nSizes <- lengths(sizeList)
  # if no "top-level" color is present, return traces untouched
  if (all(nSizes == 0)) {
    return(traces)
  }
  allSize <- unlist(compact(sizeList))
  if (!is.null(allSize) && is.discrete(allSize)) {
    stop("Size must be mapped to a numeric variable", 
         "symbols only make sense for discrete variables", call. = FALSE)
  }
  sizeRange <- range(allSize, na.rm = TRUE)
  
  types <- vapply(traces, function(tr) tr$type, character(1))
  modes <- vapply(traces, function(tr) tr$mode %||% "lines", character(1))
  hasMarker <- has_marker(types, modes)
  hasLine <- has_line(types, modes)
  hasText <- has_text(types, modes)
  
  for (i in which(nSizes > 0)) {
    sizeI <- scales::rescale(sizeList[[i]], from = sizeRange, to = traces[[1]]$sizes)
    traces[[i]]$marker <- modify_list(
      list(size = sizeI, sizemode = "area"), 
      traces[[i]]$marker
    )
    if (hasLine[[i]]) {
      warning(
        "Can't map size to lines since plotly.js doesn't yet support line.width arrays",
        call. = FALSE
      )
    }
    if (hasText[[i]]) {
      warning(
        "Can't map size to text since plotly.js doesn't yet support textfont.size arrays",
        call. = FALSE
      )
    }
  }
  traces
}

# appends a new (empty) trace to generate (plot-wide) colorbar/colorscale
map_color <- function(traces, title = "", na.color = "transparent") {
  color <- lapply(traces, function(x) {
    x[["color"]] %||% if (has_attr(x$type, "colorscale")) x[["z"]] else NULL
  })
  nColors <- lengths(color)
  # if no "top-level" color is present, return traces untouched
  if (all(nColors == 0)) {
    return(traces)
  }
  isNumeric <- vapply(color, is.numeric, logical(1))
  isDiscrete <- vapply(color, is.discrete, logical(1))
  if (any(isNumeric & isDiscrete)) {
    stop("Can't have both discrete and numeric color mappings", call. = FALSE)
  }
  # color/colorscale/colorbar attribute placement depends on trace type and marker mode
  types <- vapply(traces, function(tr) tr$type, character(1))
  modes <- vapply(traces, function(tr) tr$mode %||% "lines", character(1))
  hasMarker <- has_marker(types, modes)
  hasLine <- has_line(types, modes)
  hasText <- has_text(types, modes)
  hasZ <- has_attr(types, "colorscale") & 
    any(vapply(traces, function(tr) !is.null(tr$z), logical(1)))
  
  if (any(isNumeric)) {
    palette <- traces[[1]][["colors"]] %||% viridisLite::viridis(10)
    # TODO: use ggstat::frange() when it's on CRAN?
    allColor <- unlist(color[isNumeric])
    rng <- range(allColor, na.rm = TRUE)
    colScale <- scales::col_numeric(palette, rng, na.color = na.color)
    # generate the colorscale to be shared across traces
    vals <- if (diff(rng) > 0) {
      as.numeric(stats::quantile(allColor, probs = seq(0, 1, length.out = 25), na.rm = TRUE)) 
    } else {
      c(0, 1)
    }
    colorScale <- matrix(c(scales::rescale(vals), colScale(vals)), ncol = 2)
    colorObj <- list(
      colorbar = list(title = as.character(title), ticklen = 2),
      cmin = rng[1],
      cmax = rng[2],
      colorscale = colorScale,
      showscale = FALSE
    )
    for (i in which(isNumeric)) {
      if (hasZ[[i]]) {
        colorObj[c("cmin", "cmax")] <- NULL
        colorObj[["showscale"]] <- TRUE
        traces[[i]] <- modify_list(colorObj, traces[[i]])
        traces[[i]]$colorscale <- as_df(traces[[i]]$colorscale)
        next
      }
      colorObj$color <- color[[i]]
      if (hasLine[[i]]) {
        if (types[[i]] %in% c("scatter", "scattergl")) {
          warning("Numeric color variables cannot (yet) be mapped to lines.\n",
                  " when the trace type is 'scatter' or 'scattergl'.\n", call. = FALSE)
          traces[[i]]$mode <- paste0(traces[[i]]$mode, "+markers")
          hasMarker[[i]] <- TRUE
        }
      }                 
      if (hasMarker[[i]]) {
        traces[[i]]$marker <- modify_list(colorObj, traces[[i]]$marker)
        traces[[i]]$marker$colorscale <- as_df(traces[[i]]$marker$colorscale)
      }
      if (hasText[[i]]) {
        warning("Numeric color variables cannot (yet) be mapped to text.\n",
                "Feel free to make a feature request \n", 
                "https://github.com/plotly/plotly.js", call. = FALSE)
      }
    }
    if (any(hasZ)) return(traces) 
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
    if ("scatter3d" %in% unlist(lapply(traces, "[[", "type"))) {
      colorBarTrace$type <- "scatter3d"
      colorBarTrace$z <- range(unlist(lapply(traces, "[[", "z")), na.rm = TRUE)
    }
    traces[[length(traces) + 1]] <- structure(colorBarTrace, class = "plotly_colorbar")
  }
  
  if (any(isDiscrete)) {
    allColor <- unlist(color[isDiscrete])
    lvls <- unique(allColor)
    N <- length(lvls)
    palette <- traces[[1]][["colors"]] %||% 
      if (is.ordered(allColor)) viridisLite::viridis(N) else RColorBrewer::brewer.pal(N, "Set2")
    if (is.list(palette) && length(palette) > 1) {
      stop("Multiple numeric color palettes specified (via the colors argument).\n",
           "When using the color/colors arguments, only one palette is allowed.",
           call. = FALSE)
    }
    
    colScale <- scales::col_factor(palette, levels = lvls, na.color = na.color)
    for (i in which(isDiscrete)) {
      if (hasLine[[i]]) {
        traces[[i]]$line <- modify_list(
          list(color = colScale(color[[i]])),
          traces[[i]]$line
        )
      }                 
      if (hasMarker[[i]]) {
        traces[[i]]$marker <- modify_list(
          list(color = colScale(color[[i]])),
          traces[[i]]$marker
        )
      }
      if (hasText[[i]]) {
        traces[[i]]$textfont <- modify_list(
          list(color = colScale(color[[i]])),
          traces[[i]]$textfont
        )
      }
    }
  }
  
  traces
}

map_symbol <- function(traces) {
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
  # give a sensible ordering the valid symbols so that we map 
  # to a palette that can be easily perceived
  symbolPalette <- c(
    'circle', 'cross', 'diamond', 'square', 'triangle-down', 
    'triangle-left', 'triangle-right', 'triangle-up'
  )
  symbols <- unique(unlist(traces[[1]]$symbols)) %||% symbolPalette
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

map_linetype <- function(traces) {
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
  lvls <- if (is.factor(x)) levels(x) else unique(x)
  # the order of lvls determines the order in which traces are drawn
  # for ordered factors at least, it makes sense to draw the highest level first
  # since that _should_ be the darkest color in a sequential pallette
  if (is.ordered(x)) lvls <- rev(lvls)
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


eval_attr <- function(x, data = NULL) {
  if (lazyeval::is_formula(x)) lazyeval::f_eval(x, data) else x
}
