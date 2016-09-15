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

  # make this plot retrievable
  set_last_plot(p)
  
  layouts <- Map(function(x, y) {

    d <- plotly_data(p, y)
    x <- rapply(x, eval_attr, data = d, how = "list")
    
    # if an annotation attribute is an array, expand into multiple annotations 
    nAnnotations <- max(lengths(x$annotations) %||% 0)
    x$annotations <- purrr::transpose(lapply(x$annotations, function(x) {
      as.list(rep(x, length.out = nAnnotations))
    }))
    
    x[lengths(x) > 0]

  }, p$x$layoutAttrs, names2(p$x$layoutAttrs))

  # get rid of the data -> layout mapping 
  p$x$layoutAttrs <- NULL
  
  # accumulate, rather than override, annotations.
  annotations <- Reduce(c, c(
    if (is.null(names(p$x$layout$annotations))) p$x$layout$annotations else list(p$x$layout$annotations),
    compact(lapply(layouts, "[", "annotations"))
  ))
  # annotations shouldn't have names
  annotations <- setNames(annotations[[1]], NULL)
  
  # merge layouts into a single layout (more recent layouts will override older ones)
  p$x$layout <- modify_list(p$x$layout, Reduce(modify_list, layouts))
  p$x$layout$annotations <- annotations
  
  
  # If type was not specified in plot_ly(), it doesn't create a trace unless
  # there are no other traces
  if (is.null(p$x$attrs[[1]][["type"]])) {
    if (length(p$x$attrs) > 1 || isTRUE(attr(p, "ggplotly"))) {
      p$x$attrs[[1]] <- NULL
    }
  }

  dats <- Map(function(x, y) {

    # perform the evaluation
    dat <- plotly_data(p, y)
    trace <- structure(
      rapply(x, eval_attr, data = dat, how = "list"),
      class = oldClass(x)
    )
    
    # determine trace type (if not specified, can depend on the # of data points)
    # note that this should also determine a sensible mode, if appropriate
    trace <- verify_type(trace)
    # verify orientation of boxes/bars
    trace <- verify_orientation(trace)

    # add sensible axis names to layout
    for (i in c("x", "y", "z")) {
      nm <- paste0(i, "axis")
      idx <- which(names(trace) %in% i)
      if (length(idx) == 1) {
        title <- deparse2(x[[idx]])
        if (is3d(trace$type) || i == "z") {
          p$x$layout$scene[[nm]]$title <<- p$x$layout$scene[[nm]]$title %||% title
        } else {
          p$x$layout[[nm]]$title <<- p$x$layout[[nm]]$title %||% title
        }
      }
    }

    if (inherits(trace, c("plotly_surface", "plotly_contour"))) {
      # TODO: generate matrix for users?
      # (1) if z is vector, and x/y are null throw error
      # (2) if x/y/z are vectors and length(x) * length(y) == length(z), convert z to matrix
      if (!is.matrix(trace[["z"]]) || !is.numeric(trace[["z"]])) {
        stop("`z` must be a numeric matrix", call. = FALSE)
      }
    }

    # collect non-positional scales, plotly.js data_arrays, and "special"
    # array attributes for "data training"
    Attrs <- Schema$traces[[trace[["type"]]]]$attributes
    isArray <- lapply(Attrs, function(x) {
      tryCatch(identical(x[["valType"]], "data_array"), error = function(e) FALSE)
    })
    # I don't think we ever want mesh3d's data attrs
    dataArrayAttrs <- if (identical(trace[["type"]], "mesh3d")) NULL else names(Attrs)[as.logical(isArray)]
    # for some reason, text isn't listed as a data array attributein some traces
    # I'm looking at you scattergeo...
    tr <- trace[names(trace) %in% c(npscales(), special_attrs(trace), dataArrayAttrs, "text")]
    # TODO: does it make sense to "train" matrices/2D-tables (e.g. z)?
    tr <- tr[vapply(tr, function(x) is.null(dim(x)) && is.atomic(x), logical(1))]
    builtData <- tibble::as_tibble(tr)
    
    # avoid clobbering I() (i.e., variables that shouldn't be scaled)
    for (i in seq_along(tr)) {
      if (inherits(tr[[i]], "AsIs")) builtData[[i]] <- I(builtData[[i]])
    }

    if (NROW(builtData) > 0) {
      # Build the index used to split one "trace" into multiple traces
      isAsIs <- vapply(builtData, function(x) inherits(x, "AsIs"), logical(1))
      isDiscrete <- vapply(builtData, is.discrete, logical(1))
       # note: can only have one linetype per trace
      isSplit <- names(builtData) %in% "linetype" |
        !isAsIs & isDiscrete & names(builtData) %in% c("symbol", "color")
      if (any(isSplit)) {
        paste2 <- function(x, y) paste(x, y, sep = "<br>")
        builtData$.plotlyTraceIndex <- Reduce(paste2, builtData[isSplit])
      }
      # Build the index used to determine grouping (later on, NAs are inserted
      # via group2NA() to create the groups). This is done in 3 parts:
      # 1. Sort data by the trace index since groups are nested within traces.
      # 2. Translate missing values on positional scales to a grouping variable.
      #    If grouping isn't relevant for this trace, a warning is thrown since
      #    NAs are removed.
      # 3. The grouping from (2) and any groups detected via dplyr::groups()
      #    are combined into a single grouping variable, .plotlyGroupIndex
      builtData <- arrange_safe(builtData, ".plotlyTraceIndex")
      isComplete <- complete.cases(builtData[names(builtData) %in% c("x", "y", "z")])
      # is grouping relevant for this geometry? (e.g., grouping doesn't effect a scatterplot)
      hasGrp <- inherits(trace, paste0("plotly_", c("segment", "path", "line", "polygon"))) ||
        (grepl("scatter", trace[["type"]]) && grepl("lines", trace[["mode"]]))
      # warn about missing values if groups aren't relevant for this trace type
      if (any(!isComplete) && !hasGrp) {
        warning("Ignoring ", sum(!isComplete), " observations", call. = FALSE)
      }
      builtData$.plotlyGroupIndex <- cumsum(!isComplete)
      builtData <- builtData[isComplete, ]
      grps <- tryCatch(
        as.character(dplyr::groups(dat)),
        error = function(e) character(0)
      )
      if (length(grps) && hasGrp) {
        if (isTRUE(trace[["connectgaps"]])) {
          stop(
            "Can't use connectgaps=TRUE when data has group(s).",
            call. = FALSE
          )
        }
        builtData$.plotlyGroupIndex <- interaction(
          interaction(dat[isComplete, grps, drop = FALSE]),
          builtData$.plotlyGroupIndex %||% ""
        )
      }
      
      builtData <- arrange_safe(builtData, 
        c(".plotlyTraceIndex", ".plotlyGroupIndex", if (inherits(trace, "plotly_line")) "x")
      )
      builtData <- train_data(builtData, trace)
      trace$.plotlyVariableMapping <- names(builtData)
      
      # copy over to the trace data
      for (i in names(builtData)) {
        trace[[i]] <- builtData[[i]]
      }
    }

    # TODO: provide a better way to clean up "high-level" attrs
    trace[c("ymin", "ymax", "yend", "xend")] <- NULL

    trace[lengths(trace) > 0]

  }, p$x$attrs, names2(p$x$attrs))

  # traceify by the interaction of discrete variables
  traces <- list()
  for (i in seq_along(dats)) {
    d <- dats[[i]]
    scaleAttrs <- names(d) %in% paste0(npscales(), "s")
    traces <- c(traces, traceify(d[!scaleAttrs], d$.plotlyTraceIndex))
    if (i == 1) traces[[1]] <- c(traces[[1]], d[scaleAttrs])
  }

  # insert NAs to differentiate groups
  traces <- lapply(traces, function(x) {
    d <- data.frame(x[names(x) %in% x$.plotlyVariableMapping], stringsAsFactors = FALSE)
    d <- group2NA(
      d, ".plotlyGroupIndex", ordered = if (inherits(x, "plotly_line")) "x",
      retrace.first = inherits(x, "plotly_polygon")
    )
    for (i in x$.plotlyVariableMapping) {
      # try to reduce the amount of data we have to send for non-positional scales
      x[[i]] <- structure(
        if (i %in% npscales()) uniq(d[[i]]) else d[[i]],
        class = oldClass(x[[i]])
      )
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
      "alpha", npscales(), paste0(npscales(), "s"),
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

  # polar charts don't like null width/height keys 
  if (is.null(p$x$layout[["height"]])) p$x$layout[["height"]] <- NULL
  if (is.null(p$x$layout[["width"]])) p$x$layout[["width"]] <- NULL
  
  # ensure we get the order of categories correct
  # (plotly.js uses the order in which categories appear by default)
  p <- populate_categorical_axes(p)
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
  p <- verify_webgl(p)
  p
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
    idx <- rep(seq_len(NROW(data)), each = 2)
    dat <- as.data.frame(data[!grepl("^xend$|^yend", names(data))])
    dat <- dat[idx, ]
    idx2 <- seq.int(2, NROW(dat), by = 2)
    dat[idx2, "x"] <- data[["xend"]]
    dat[idx2, "y"] <- data[["yend"]]
    data <- dplyr::group_by_(dat, ".plotlyGroupIndex", add = TRUE)
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

  types <- unlist(lapply(traces, function(tr) tr$type %||% "scatter"))
  modes <- unlist(lapply(traces, function(tr) tr$mode %||% "lines"))
  hasMarker <- has_marker(types, modes)
  hasLine <- has_line(types, modes)
  hasText <- has_text(types, modes)

  for (i in which(nSizes > 0)) {
    s <- sizeList[[i]]
    isConstant <- inherits(s, "AsIs")
    sizeI <- if (isConstant) {
      structure(s, class = setdiff(class(s), "AsIs"))
    } else {
      scales::rescale(s, from = sizeRange, to = traces[[1]]$sizes)
    }
    if (hasMarker[[i]]) {
      traces[[i]]$marker <- modify_list(
        list(size = sizeI, sizemode = "area"),
        traces[[i]]$marker
      )
    }
    if (hasLine[[i]]) {
      if (!isConstant || length(sizeI) > 1) {
        warning(
          "plotly.js doesn't yet support line.width arrays, track this issue for progress\n",
          "  https://github.com/plotly/plotly.js/issues/147",
          call. = FALSE
        )
      }
      traces[[i]]$line <- modify_list(
        list(width = sizeI[1]),
        traces[[i]]$line
      )
    }
    if (hasText[[i]]) {
      if (!isConstant || length(sizeI) > 1) {
        warning(
          "plotly.js doesn't yet support textfont.size arrays",
          call. = FALSE
        )
      }
      traces[[i]]$textfont <- modify_list(
        list(size = sizeI[1]),
        traces[[i]]$textfont
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
  isConstant <- vapply(color, function(x) inherits(x, "AsIs") || is.null(x), logical(1))
  isNumeric <- vapply(color, is.numeric, logical(1)) & !isConstant
  isDiscrete <- vapply(color, is.discrete, logical(1)) & !isConstant
  if (any(isNumeric & isDiscrete)) {
    stop("Can't have both discrete and numeric color mappings", call. = FALSE)
  }
  # color/colorscale/colorbar attribute placement depends on trace type and marker mode
  types <- unlist(lapply(traces, function(tr) tr$type %||% "scatter"))
  modes <- unlist(lapply(traces, function(tr) tr$mode %||% "lines"))
  hasMarker <- has_marker(types, modes)
  hasLine <- has_line(types, modes)
  hasText <- has_text(types, modes)
  hasZ <- has_attr(types, "colorscale") &
    any(vapply(traces, function(tr) !is.null(tr$z), logical(1)))

  colorDefaults <- traceColorDefaults()
  for (i in which(isConstant)) {
    # https://github.com/plotly/plotly.js/blob/c83735/src/plots/plots.js#L58
    idx <- i %% length(colorDefaults) + i %/% length(colorDefaults)
    col <- color[[i]] %||% colorDefaults[[idx]]
    alpha <- traces[[i]]$alpha %||% 1
    rgb <- toRGB(col, alpha)
    obj <- if (hasLine[[i]]) "line" else if (hasMarker[[i]]) "marker" else if (hasText[[i]]) "textfont"
    traces[[i]][[obj]] <- modify_list(list(color = rgb), traces[[i]][[obj]])
    traces[[i]][[obj]] <- modify_list(list(fillcolor = rgb), traces[[i]][[obj]])
  }

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
    colorScale <- matrix(
      c(scales::rescale(vals), toRGB(colScale(vals), traces[[1]]$alpha %||% 1)),
      ncol = 2
    )
    colorObj <- list(
      colorbar = Reduce(modify_list, lapply(traces, function(x) x$marker[["colorbar"]])) %||%
        list(title = as.character(title), ticklen = 2),
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
        # sigh, contour colorscale doesn't support alpha
        if (traces[[i]][["type"]] == "contour") {
          traces[[i]]$colorscale[, 2] <- strip_alpha(traces[[i]]$colorscale[, 2])
        }
        next
      }
      colorObj$color <- color[[i]]
      if (hasLine[[i]]) {
        if (types[[i]] %in% c("scatter", "scattergl")) {
          warning("Numeric color variables cannot (yet) be mapped to lines.\n",
                  " when the trace type is 'scatter' or 'scattergl'.\n", call. = FALSE)
          traces[[i]]$mode <- paste0(traces[[i]]$mode, "+markers")
          hasMarker[[i]] <- TRUE
        } else {
          # scatter3d supports data arrays for color
          traces[[i]][["line"]] <- modify_list(colorObj, traces[[i]][["line"]])
          traces[[i]]$marker$colorscale <- as_df(traces[[i]]$marker$colorscale)
        }
      }
      if (hasMarker[[i]]) {
        traces[[i]][["marker"]] <- modify_list(colorObj, traces[[i]][["marker"]])
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
    # yay for consistency plotly.js
    if ("scatter3d" %in% types) {
      colorBarTrace$type <- "scatter3d"
      colorBarTrace$z <- range(unlist(lapply(traces, "[[", "z")), na.rm = TRUE)
    }
    if (length(type <- intersect(c("scattergeo", "scattermapbox"), types))) {
      colorBarTrace$type <- type
      colorBarTrace$lat <- range(unlist(lapply(traces, "[[", "lat")), na.rm = TRUE)
      colorBarTrace$lon <- range(unlist(lapply(traces, "[[", "lon")), na.rm = TRUE)
      colorBarTrace[["x"]] <- NULL
      colorBarTrace[["y"]] <- NULL
    }
    traces[[length(traces) + 1]] <- structure(colorBarTrace, class = "plotly_colorbar")
  }

  if (any(isDiscrete)) {
    # unlist() does _not_ preserve order factors
    isOrdered <- all(vapply(color[isDiscrete], is.ordered, logical(1)))
    allColor <- unlist(color[isDiscrete])
    lvls <- getLevels(allColor)
    N <- length(lvls)
    pal <- traces[[1]][["colors"]] %||%
      if (isOrdered) viridisLite::viridis(N) else RColorBrewer::brewer.pal(N, "Set2")
    colScale <- scales::col_factor(pal, levels = names(pal) %||% lvls, na.color = na.color)
    for (i in which(isDiscrete)) {
      rgb <- toRGB(colScale(as.character(color[[i]])), traces[[i]]$alpha %||% 1)
      obj <- if (hasLine[[i]]) "line" else if (hasMarker[[i]]) "marker" else if (hasText[[i]]) "textfont"
      traces[[i]][[obj]] <- modify_list(list(color = rgb), traces[[i]][[obj]])
      # match the plotly.js default of half transparency in the fill color
      traces[[i]][[obj]] <- modify_list(list(fillcolor = toRGB(rgb, 0.5)), traces[[i]][[obj]])
    }
  }
  
  # marker.line.color (stroke) inherits from marker.color (color)
  # TODO: allow users to control via a `stroke`` argument
  # to make consistent, in "filled polygons", color -> fillcolor, stroke -> line.color 
  for (i in seq_along(color)) {
    if (!is.null(traces[[i]]$marker$color)) {
      traces[[i]]$marker$line$color <- traces[[i]]$marker$line$color %||% "transparent"
      for (j in c("error_x", "error_y")) {
        if (!is.null(traces[[i]][[j]])) {
          traces[[i]][[j]][["color"]] <- traces[[i]][[j]][["color"]] %||%
            traces[[i]]$marker[["color"]]
        }
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
  lvls <- getLevels(symbol)
  # get a sensible default palette (also throws warnings)
  pal <- setNames(scales::shape_pal()(length(lvls)), lvls)
  pal <- supplyUserPalette(pal, traces[[1]][["symbols"]])

  validSymbols <- as.character(Schema$traces$scatter$attributes$marker$symbol$values)

  for (i in which(nSymbols > 0)) {
    s <- symbolList[[i]]
    symbols <- pch2symbol(if (inherits(s, "AsIs")) s else as.character(pal[as.character(s)]))
    illegalSymbols <- setdiff(symbols, validSymbols)
    if (length(illegalSymbols)) {
      warning(
        "The following are not valid symbol codes:\n'",
        paste(illegalSymbols, collapse = "', '"), "'\n",
        "Valid symbols include:\n'",
        paste(validSymbols, collapse = "', '"), call. = FALSE
      )
    }
    traces[[i]][["marker"]] <- modify_list(
      list(symbol = symbols), traces[[i]][["marker"]]
    )
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
  lvls <- getLevels(linetype)
  # get a sensible default palette
  pal <- setNames(scales::linetype_pal()(length(lvls)), lvls)
  pal <- supplyUserPalette(pal, traces[[1]][["linetypes"]])

  validLinetypes <- as.character(Schema$traces$scatter$attributes$line$dash$values)
  if (length(pal) > length(validLinetypes)) {
    warning("plotly.js only supports 6 different linetypes", call. = FALSE)
  }

  for (i in which(nLinetypes > 0)) {
    l <- linetypeList[[i]]
    dashes <- lty2dash(if (inherits(l, "AsIs")) l else as.character(pal[as.character(l)]))
    illegalLinetypes <- setdiff(dashes, validLinetypes)
    if (length(illegalLinetypes)) {
      warning(
        "The following are not valid linetype codes:\n'",
        paste(illegalLinetypes, collapse = "', '"), "'\n",
        "Valid linetypes include:\n'",
        paste(validLinetypes, collapse = "', '"), "'", call. = FALSE
      )
    }
    traces[[i]][["line"]] <- modify_list(
      list(dash = dashes), traces[[i]][["line"]]
    )
    # ensure the mode is set so that the linetype is relevant
    if (!grepl("lines", traces[[i]]$mode %||% "")) {
      message("Adding lines to mode; otherwise linetype would have no effect.")
      traces[[i]][["mode"]] <- paste0(traces[[i]][["mode"]], "+lines")
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

# overwrite defaults with the user defined palette
supplyUserPalette <- function(default, user) {
  for (i in seq_along(user)) {
    idx <- names(user)[i] %||% i
    default[idx] <- user[i]
  }
  default
}
