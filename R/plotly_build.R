#' 'Build' (i.e., evaluate) a plotly object
#'
#' This generic function creates the list object sent to plotly.js
#' for rendering. Using this function can be useful for overriding defaults
#' provided by `ggplotly`/`plot_ly` or for debugging rendering
#' errors.
#'
#' @param p a ggplot object, or a plotly object, or a list.
#' @param registerFrames should a frame trace attribute be interpreted as frames in an animation?
#' @export
#' @examples
#'
#' p <- plot_ly(economics, x = ~date, y = ~pce)
#' # the unevaluated plotly object
#' str(p)
#' # the evaluated data
#' str(plotly_build(p)$x$data)
#'
plotly_build <- function(p, registerFrames = TRUE) {
  UseMethod("plotly_build")
}

#' @export
plotly_build.NULL <- function(...) {
  htmltools::browsable(htmltools::div(...))
}

#' @export
plotly_build.list <- function(p, registerFrames = TRUE) {
  plotly_build(as_widget(p))
}

#' @export
plotly_build.gg <- function(p, registerFrames = TRUE) {
  # note: since preRenderHook = plotly_build in as_widget(),
  # plotly_build.plotly() will be called on gg objects as well
  plotly_build(ggplotly(p))
}

#' @export
plotly_build.plotly <- function(p, registerFrames = TRUE) {
  
  # make this plot retrievable
  set_last_plot(p)
  
  layouts <- Map(function(x, y) {
    
    d <- plotly_data(p, y)
    x <- rapply(x, eval_attr, data = d, how = "list")
    
    # if an annotation attribute is an array, expand into multiple annotations
    nAnnotations <- max(lengths(x$annotations) %||% 0)
    if (!is.null(names(x$annotations))) {
      # font is the only list object, so store it, and attach after transposing
      font <- x$annotations[["font"]]
      x$annotations <- purrr::transpose(lapply(x$annotations, function(x) {
        as.list(rep(x, length.out = nAnnotations))
      }))
      for (i in seq_len(nAnnotations)) {
        x$annotations[[i]][["font"]] <- font
      }
    }
    
    x[lengths(x) > 0]
    
  }, p$x$layoutAttrs, names2(p$x$layoutAttrs))
  
  # get rid of the data -> layout mapping
  p$x$layoutAttrs <- NULL
  
  # accumulate, rather than override, annotations.
  annotations <- Reduce(c, c(
    list(p$x$layout$annotations),
    setNames(compact(lapply(layouts, "[[", "annotations")), NULL)
  ))
  
  # merge layouts into a single layout (more recent layouts will override older ones)
  p$x$layout <- modify_list(p$x$layout, Reduce(modify_list, layouts))
  p$x$layout$annotations <- annotations
  
  # keep frame mapping for populating layout.slider.currentvalue in animations
  frameMapping <- unique(unlist(
    lapply(p$x$attrs, function(x) deparse2(x[["frame"]])),
    use.names = FALSE
  ))
  if (length(frameMapping) > 1) {
    warning("Only one `frame` variable is allowed", call. = FALSE)
  }
  
  # Attributes should be NULL if none exist (rather than an empty list)
  if (length(p$x$attrs) == 0) p$x$attrs <- NULL
  
  # If there is just one (unevaluated) trace, and the data is sf, add an sf layer
  if (length(p$x$attrs) == 1 && !inherits(p$x$attrs[[1]], "plotly_eval") && is_sf(plotly_data(p))) {
    p <- add_sf(p)
  }
  
  # If type was not specified in plot_ly(), it doesn't create a trace unless
  # there are no other traces
  if (is.null(p$x$attrs[[1]][["type"]]) && length(p$x$attrs) > 1) {
    p$x$attrs[[1]] <- NULL
  }
  
  # have the attributes already been evaluated?
  is.evaled <- function(x) inherits(x, "plotly_eval")
  attrsToEval <- p$x$attrs[!vapply(p$x$attrs, is.evaled, logical(1))]
  
  # trace type checking and renaming for plot objects
  if (is_mapbox(p) || is_geo(p)) {
    p <- geo2cartesian(p)
    attrsToEval <- lapply(attrsToEval, function(tr) {
      if (!grepl("scatter|choropleth", tr[["type"]] %||% "scatter")) {
        stop("Cant add a '", tr[["type"]], "' trace to a map object", call. = FALSE)
      }
      if (is_mapbox(p)) tr[["type"]] <- "scattermapbox"
      if (is_geo(p)) {
        tr[["type"]] <- if (!is.null(tr[["z"]])) "choropleth" else "scattergeo"
      }
      tr
    })
  }

  dats <- Map(function(x, y) {
    
    # grab the data for this trace
    dat <- plotly_data(p, y)
    
    # formula/symbol/attribute evaluation
    trace <- structure(
      rapply(x, eval_attr, data = dat, how = "list"),
      class = oldClass(x)
    )
    
    # determine trace type (if not specified, can depend on the # of data points)
    # note that this should also determine a sensible mode, if appropriate
    trace <- verify_type(trace)
    # verify orientation of boxes/bars
    trace <- verify_orientation(trace)
    
    # attach crosstalk info, if necessary
    if (crosstalk_key() %in% names(dat) && isTRUE(trace[["inherit"]] %||% TRUE)) {
      trace[["key"]] <- trace[["key"]] %||% dat[[crosstalk_key()]]
      trace[["set"]] <- trace[["set"]] %||% attr(dat, "set")
    }
    
    # if appropriate, tack on a group index
    grps <- if (has_group(trace)) tryNULL(dplyr::group_vars(dat))
    if (length(grps) && any(lengths(trace) == NROW(dat))) {
      trace[[".plotlyGroupIndex"]] <- interaction(dat[, grps, drop = F])
    }
    
    # add sensible axis names to layout
    for (i in c("x", "y", "z")) {
      nm <- paste0(i, "axis")
      idx <- which(names(trace) %in% i)
      if (length(idx) == 1) {
        title <- default(deparse2(x[[idx]]))
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
    isArray <- vapply(Attrs, function(x) {
      tryFALSE(identical(x[["valType"]], "data_array"))
    }, logical(1))
    arrayOk <- vapply(Attrs, function(x) tryNULL(x[["arrayOk"]]) %||% FALSE, logical(1))
    # "non-tidy" traces allow x/y of different lengths, so ignore those
    dataArrayAttrs <- if (is_tidy(trace)) names(Attrs)[isArray | arrayOk]
    allAttrs <- c(
      dataArrayAttrs, special_attrs(trace), npscales(), "frame",
      # for some reason, text isn't listed as a data array in some traces
      # I'm looking at you scattergeo...
      ".plotlyGroupIndex", "text", "key", "fillcolor", "name", "legendgroup"
    )
    tr <- trace[names(trace) %in% allAttrs]
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
      isSplit <- names(builtData) %in% c("split", "linetype", "frame", "fillcolor", "name") |
        !isAsIs & isDiscrete & names(builtData) %in% c("symbol", "color")
      if (any(isSplit)) {
        paste2 <- function(x, y) if (identical(x, y)) x else paste(x, y, sep = br())
        splitVars <- builtData[isSplit]
        builtData[[".plotlyTraceIndex"]] <- Reduce(paste2, splitVars)
        # in registerFrames() we need to strip the frame from .plotlyTraceIndex
        # so keep track of which variable it is...
        trace$frameOrder <- which(names(splitVars) %in% "frame")
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
      # warn about missing values if groups aren't relevant for this trace type
      if (any(!isComplete) && !has_group(trace)) {
        warning("Ignoring ", sum(!isComplete), " observations", call. = FALSE)
      }
      builtData[[".plotlyMissingIndex"]] <- cumsum(!isComplete)
      builtData <- builtData[isComplete, ]
      if (length(grps) && has_group(trace) && isTRUE(trace[["connectgaps"]])) {
        stop(
          "Can't use connectgaps=TRUE when data has group(s).", call. = FALSE
        )
      }
      builtData[[".plotlyGroupIndex"]] <- interaction(
        builtData[[".plotlyGroupIndex"]] %||% "",
        builtData[[".plotlyMissingIndex"]]
      )
      builtData <- arrange_safe(builtData,
                                c(".plotlyTraceIndex", ".plotlyGroupIndex",
                                  if (inherits(trace, "plotly_line")) "x")
      )
      builtData <- train_data(builtData, trace)
      trace[[".plotlyVariableMapping"]] <- names(builtData)
      # copy over to the trace data
      for (i in names(builtData)) {
        trace[[i]] <- builtData[[i]]
      }
    }
    # TODO: provide a better way to clean up "high-level" attrs
    trace[c("ymin", "ymax", "yend", "xend")] <- NULL
    trace[lengths(trace) > 0]
    
  }, attrsToEval, names2(attrsToEval))
  
  p$x$attrs <- lapply(p$x$attrs, function(x) structure(x, class = "plotly_eval"))
  
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
      d, if (has_group(x)) ".plotlyGroupIndex",
      ordered = if (inherits(x, "plotly_line")) "x",
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
  
  # Map special plot_ly() arguments to plotly.js trace attributes.
  # Note that symbol/linetype can modify the mode, so those are applied first
  # TODO: use 'legends 2.0' to create legends for these discrete mappings
  # https://github.com/plotly/plotly.js/issues/1668
  if (length(traces)) {
    traces <- map_symbol(traces)
    traces <- map_linetype(traces)
    traces <- map_size(traces)
    traces <- map_size(traces, stroke = TRUE) #i.e., span
    colorTitle <- unlist(lapply(p$x$attrs, function(x) { deparse2(x[["color"]] %||% x[["z"]]) }))
    strokeTitle <- unlist(lapply(p$x$attrs, function(x) deparse2(x[["stroke"]])))
    traces <- map_color(traces, title = paste(colorTitle, collapse = br()), colorway = colorway(p))
    traces <- map_color(traces, stroke = TRUE, title = paste(strokeTitle, collapse = br()), colorway = colorway(p))
  }
  
  for (i in seq_along(traces)) {
    # remove special mapping attributes
    mappingAttrs <- c(
      "alpha", "alpha_stroke", npscales(), paste0(npscales(), "s"),
      ".plotlyGroupIndex", ".plotlyMissingIndex",
      ".plotlyTraceIndex", ".plotlyVariableMapping", "inherit"
    )
    for (j in mappingAttrs) {
      traces[[i]][[j]] <- NULL
    }
  }
  
  # .crossTalkKey -> key
  traces <- lapply(traces, function(x) {
    setNames(x, sub(crosstalk_key(), "key", names(x), fixed = TRUE))
  })
  
  # it's possible that the plot object already has some traces
  # (like figures pulled from a plotly server)
  p$x$data <- setNames(c(p$x$data, traces), NULL)
  
  # supply linked highlighting options/features
  p <- supply_highlight_attrs(p)
  
  # supply trace anchor and domain information
  p <- supply_defaults(p)
  
  # attribute naming corrections for "geo-like" traces
  p <- cartesian2geo(p)
  
  # Compute sensible bounding boxes for each mapbox/geo subplot
  p <- fit_bounds(p)
  
  # polar charts don't like null width/height keys
  if (is.null(p$x$layout[["height"]])) p$x$layout[["height"]] <- NULL
  if (is.null(p$x$layout[["width"]])) p$x$layout[["width"]] <- NULL
  
  # ensure we get the order of categories correct
  # (plotly.js uses the order in which categories appear by default)
  p <- populate_categorical_axes(p)
  # translate '\n' to '<br />' in text strings
  p <- translate_linebreaks(p)
  # if it makes sense, add markers/lines/text to mode
  p <- verify_mode(p)
  # annotations & shapes must be an array of objects
  # TODO: should we add anything else to this?
  p <- verify_arrays(p)
  # set a sensible hovermode if it hasn't been specified already
  p <- verify_hovermode(p)
  # try to convert to webgl if toWebGl was used
  p <- verify_webgl(p)
  # crosstalk dynamically adds traces, meaning that a legend could be dynamically
  # added, which is confusing. So here we populate a sensible default.
  p <- verify_showlegend(p)
  
  # NOTE: this needs to occur *before* registering frames so simple/nested key
  # flags get passed onto frame data.
  p <- verify_key_type(p)
  
  if (registerFrames) {
    p <- registerFrames(p, frameMapping = frameMapping)
  }
  
  p <- verify_guides(p)
  
  # verify plot attributes are legal according to the plotly.js spec
  p <- verify_attr_names(p)
  # box up 'data_array' attributes where appropriate
  p <- verify_attr_spec(p)
  
  # make sure we're including mathjax (if TeX() is used)
  p <- verify_mathjax(p)
  
  # if a partial bundle was specified, make sure it supports the visualization
  p <- verify_partial_bundle(p)
  
  # scattergl currently doesn't render in RStudio on Windows
  # https://github.com/ropensci/plotly/issues/1214
  p <- verify_scattergl_platform(p)
  
  # make sure plots don't get sent out of the network (for enterprise)
  p$x$base_url <- get_domain()
  p
}

# ----------------------------------------------------------------
# Functions used solely within plotly_build
# ----------------------------------------------------------------

registerFrames <- function(p, frameMapping = NULL) {
  # ensure one frame value per trace, and if its missing, insert NA
  p$x$data <- lapply(p$x$data, function(tr) {
    tr[["frame"]] <- tr[["frame"]][[1]] %||% NA
    tr
  })
  
  # the ordering of this object determines the ordering of the frames
  frameAttrs <- unlist(lapply(p$x$data, "[[", "frame"))
  # NOTE: getLevels() should drop NAs
  frameNames <- getLevels(frameAttrs)
  p$x$data <- lapply(p$x$data, function(tr) { tr$frame <- as.character(tr$frame); tr })
  
  # remove frames from the trace names
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    if (length(tr[["name"]]) != 1) next
    nms <- strsplit(as.character(tr[["name"]]), br())[[1]]
    idx <- setdiff(seq_along(nms), tr$frameOrder %||% 0)
    p$x$data[[i]]$name <- if (length(idx)) paste(nms[idx], collapse = br()) else NULL
    p$x$data[[i]]$frameOrder <- NULL
  }
  
  # exit in trivial cases
  nFrames <- length(frameNames)
  if (nFrames < 2) return(p)
  
  # --------------------------------------------------------------------------
  # set a "global" range of x/y (TODO: handle multiple axes?)
  # --------------------------------------------------------------------------
  
  x <- unlist(lapply(p$x$data, function(x) x[["x"]]))
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    if (identical(p$x$layout$xaxis$type, "log")) {
      rng <- log10(rng)
      rng[is.nan(rng)] <- 0
    }
    p$x$layout$xaxis$range <- p$x$layout$xaxis$range %||% extendrange(rng)
  }
  y <- unlist(lapply(p$x$data, function(x) x[["y"]]))
  if (is.numeric(y)) {
    rng <- range(y, na.rm = TRUE)
    if (identical(p$x$layout$yaxis$type, "log")) {
      rng <- log10(rng)
      rng[is.nan(rng)] <- 0
    }
    p$x$layout$yaxis$range <- p$x$layout$yaxis$range %||% extendrange(rng)
  }
  
  # --------------------------------------------------------------------------
  # Similar to setting a global x/y range, we need a "global trace range"
  # 
  # implementation details via @rreusser: frames specify *state changes*, 
  # so if frame 1 has 3 traces, and frame 2 has 2 traces, 
  # we need to explicity supply 3 traces
  # in both frames, but make 1 invisible in frame 2. For example,
  # http://codepen.io/cpsievert/pen/gmXVWe
  # For that reason, every frame (including the "initial" frame) has the 
  # max # of traces and "missing traces" are not visible (i.e., `visible=false`)
  # --------------------------------------------------------------------------
  
  # remember, at this point, frame has been removed from the trace name
  frameTraceNames <- unique(unlist(lapply(p$x$data[!is.na(frameAttrs)], "[[", "name")))
  for (i in seq_along(frameNames)) {
    nm <- frameNames[[i]]
    d <- p$x$data[sapply(p$x$data, "[[", "frame") %in% nm]
    
    # ensure, the frames API knows what is visible/invisible
    d <- lapply(d, function(tr) { tr$visible <- tr$visible %||% TRUE; tr })
    
    # if this frame is missing a trace name, supply an invisible one
    traceNamesMissing <- setdiff(frameTraceNames, sapply(d, "[[", "name"))
    for (j in traceNamesMissing) {
      idx <- vapply(p$x$data, function(tr) isTRUE(tr[["name"]] == j), logical(1))
      idx <- which(idx)[[1]]
      invisible <- modify_list(p$x$data[[idx]], list(visible = FALSE))
      d <- c(d, list(invisible))
    }
    p$x$frames[[i]] <- list(
      name = as.character(format(nm)),
      data = lapply(d, function(tr) { 
        spec <- Schema$traces[[tr$type %||% "scatter"]]$attributes
        verify_attr(tr, spec)
      })
    )
  }
  
  # ensure the plot knows about the "global trace range"
  firstFrame <- vapply(p$x$data, function(tr) isTRUE(tr[["frame"]] %in% frameNames[[1]]), logical(1))
  p$x$data[firstFrame] <- p$x$frames[[1]]$data
  
  # remove frame traces
  idx <- vapply(p$x$data, function(tr) isTRUE(tr[["frame"]] %in% frameNames[-1]), logical(1))
  p$x$data[idx] <- NULL
  
  # this works since we now have a global trace range
  p$x$frames <- lapply(p$x$frames, function(f) {
    f$traces <- i(which(!is.na(sapply(p$x$data, "[[", "frame"))) - 1)
    f
  })
  
  # retrain color defaults
  p$x$data <- colorway_retrain(p$x$data, colorway(p))
  p$x$frames <- lapply(p$x$frames, function(f) {
    f$data <- colorway_retrain(f$data, colorway(p)[f$traces + 1])
    f
  })
  
  # populate layout.sliders.currentvalue with a sensible default
  defaultvalue <- if (length(frameMapping) == 1) {
    list(
      prefix = paste0(frameMapping, ": "),
      xanchor = 'right',
      font = list(
        size = 16,
        color = toRGB("gray80")
      )
    )
  } else NULL
  
  # supply animation option defaults (a la, highlight_defaults())
  p$animation <- p$animation %||% animation_opts_defaults()
  
  # if all the frame trace data are scatter traces, set a default of redraw=F
  types <- unique(unlist(lapply(p$x$frames, function(f) {
    vapply(f$data, function(tr) tr$type %||% "scatter", character(1))
  })))
  if (identical(types, "scatter") && is.default(p$animation$frame$redraw)) {
    p$animation$frame$redraw <- default(FALSE)
  }
  
  # _always_ display an animation button and slider by default
  animation_button_supply(
    animation_slider_supply(p, currentvalue = defaultvalue)
  )
}


train_data <- function(data, trace) {
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


map_size <- function(traces, stroke = FALSE) {
  sizeList <- lapply(traces, "[[", if (stroke) "span" else "size")
  nSizes <- lengths(sizeList)
  # if no "top-level" size is present, return traces untouched
  if (all(nSizes == 0)) return(traces)
  
  allSize <- unlist(compact(sizeList))
  if (!is.null(allSize) && is.discrete(allSize)) {
    stop("`size`/`width` values must be numeric .", call. = FALSE)
  }
  sizeRange <- range(allSize, na.rm = TRUE)
  
  mapSize <- switch(
    if (stroke) "span" else "size",
    span = function(trace, sizes) {
      type <- trace$type %||% "scatter"
      size_ <- uniq(sizes)
      isSingular <- length(size_) == 1
      attrs <- Schema$traces[[type]]$attributes
      
      # `span` controls marker.line.width
      if (has_attr(type, "marker")) {
        s <- if (isSingular) size_ else if (array_ok(attrs$marker$line$width)) sizes
        trace$marker$line <- modify_list(list(width = default(s)), trace$marker$line)
      }
      
      # `span` controls error_[x/y].thickness 
      for (attr in c("error_y", "error_x")) {
        if (!has_attr(type, attr)) next
        s <- if (isSingular) size_ else if (array_ok(attrs[[attr]]$thickness)) sizes
        trace[[attr]] <- modify_list(list(thickness = default(s)), trace[[attr]])
      }
      
      # When fill exists, `span` controls line.width
      if (has_fill(trace) && has_attr(type, "line")) {
        s <- if (isSingular) size_ else if (array_ok(attrs$line$width)) sizes else NA
        if (is.na(s)) {
          warning("`line.width` does not currently support multiple values.", call. = FALSE)
        } else {
          trace[["line"]] <- modify_list(list(width = default(s)), trace[["line"]])
        }
      }
      
      trace
    }, 
    size = function(trace, sizes) {
      type <- trace$type %||% "scatter"
      size_ <- uniq(sizes)
      isSingular <- length(size_) == 1
      attrs <- Schema$traces[[type]]$attributes
      
      # `size` controls marker.size (note 'bar' traces have marker but not marker.size)
      # TODO: always ensure an array? https://github.com/ropensci/plotly/pull/1176
      if (has_attr(type, "marker") && "size" %in% names(attrs$marker)) {
        s <- if (isSingular) size_ else if (array_ok(attrs$marker$size)) sizes
        trace$marker <- modify_list(list(size = default(s), sizemode = default("area")), trace$marker)
      }
      
      # `size` controls textfont.size
      if (has_attr(type, "textfont")) {
        s <- if (isSingular) size_ else if (array_ok(attrs$textfont$size)) sizes
        trace$textfont <- modify_list(list(size = default(s)), trace$textfont)
      }
      
      # `size` controls error_[x/y].width 
      for (attr in c("error_y", "error_x")) {
        if (!has_attr(type, attr)) next
        s <- if (isSingular) size_ else if (array_ok(attrs[[attr]]$width)) sizes
        trace[[attr]] <- modify_list(list(width = default(s)), trace[[attr]])
      }
      
      # if fill does not exist, `size` controls line.width
      if (!has_fill(trace) && has_attr(type, "line")) {
        s <- if (isSingular) size_ else if (array_ok(attrs$line$width)) sizes else NA
        if (is.na(s)) {
          warning("`line.width` does not currently support multiple values.", call. = FALSE)
        } else {
          trace[["line"]] <- modify_list(list(width = default(s)), trace[["line"]])
        }
      }
      trace
    }
  )
  
  for (i in which(nSizes > 0)) {
    s <- sizeList[[i]]
    isConstant <- inherits(s, "AsIs")
    sizeI <- if (isConstant) {
      structure(s, class = setdiff(class(s), "AsIs"))
    } else {
      to <- if (stroke) traces[[1]][["spans"]] else traces[[1]][["sizes"]]
      scales::rescale(s, from = sizeRange, to = to)
    }
    traces[[i]] <- mapSize(traces[[i]], sizeI)
  }
  traces
}

# appends a new (empty) trace to generate (plot-wide) colorbar/colorscale
map_color <- function(traces, stroke = FALSE, title = "", colorway, na.color = "transparent") {
  color <- if (stroke) {
    lapply(traces, function(x) { x[["stroke"]] %||% x[["color"]] })
  } else {
    lapply(traces, function(x) { x[["color"]] %||% if (grepl("histogram2d", x[["type"]])) c(0, 1) else if (has_attr(x[["type"]], "colorscale")) x[["surfacecolor"]] %||% x[["z"]] })
  }
  alphas <- if (stroke) {
    vapply(traces, function(x) x$alpha_stroke %||% 1, numeric(1)) 
  } else {
    vapply(traces, function(x) x[["alpha"]] %||% 1, numeric(1)) 
  }
  
  isConstant <- vapply(color, function(x) inherits(x, "AsIs") || is.null(x), logical(1))
  isNumeric <- vapply(color, is.numeric, logical(1)) & !isConstant
  isDiscrete <- vapply(color, is.discrete, logical(1)) & !isConstant
  if (any(isNumeric & isDiscrete)) stop("Can't have both discrete and numeric color mappings", call. = FALSE)
  uniqColor <- lapply(color, uniq)
  isSingular <- lengths(uniqColor) == 1
  
  # color/colorscale/colorbar attribute placement depends on trace type and marker mode
  # TODO: remove these and make numeric colorscale mapping more like the rest
  types <- vapply(traces, function(tr) tr$type %||% "scatter", character(1))
  modes <- vapply(traces, function(tr) tr$mode %||% "lines", character(1))
  hasLine <- has_line(types, modes)
  hasLineColor <- has_color_array(types, "line")
  hasText <- has_text(types, modes)
  hasTextColor <- has_color_array(types, "text")
  hasZ <- has_attr(types, "colorscale") & !stroke &
    any(vapply(traces, function(tr) {
      !is.null(tr[["z"]]) || grepl("histogram2d", tr[["type"]])
    }, logical(1)))
  
  # IDEA - attach color codes whether they make sense, unless there is a 
  # vector of color codes and the target is a constant
  mapColor <- switch(
    if (stroke) "stroke" else "fill",
    stroke = function(trace, rgba, is_colorway = FALSE) {
      type <- trace$type %||% "scatter"
      rgba_ <- uniq(rgba)
      isSingular <- length(rgba_) == 1
      attrs <- Schema$traces[[type]]$attributes
      default_ <- if (is_colorway) function(x) prefix_class(default(x), "colorway") else default
      
      if (has_attr(type, "marker")) {
        col <- if (isSingular) rgba_ else if (array_ok(attrs$marker$line$color)) rgba
        trace$marker$line <- modify_list(list(color = default_(col)), trace$marker$line)
      }
      if (has_fill(trace)) {
        col <- if (isSingular) rgba_ else if (array_ok(attrs$line$color)) rgba
        if (is.null(col)) {
          warning("`line.color` does not currently support multiple values.", call. = FALSE)
        } else {
          trace$line <- modify_list(list(color = default_(col)), trace$line)
        }
      }
      trace
    },
    fill = function(trace, rgba, is_colorway = FALSE) {
      type <- trace$type %||% "scatter"
      rgba_ <- uniq(rgba)
      isSingular <- length(rgba_) == 1
      attrs <- Schema$traces[[type]]$attributes
      default_ <- if (is_colorway) function(x) prefix_class(default(x), "colorway") else default
      
      # `color` controls marker.color, textfont.color, error_[x/y].color
      # TODO: any more attributes that make sense to include here?
      for (attr in c("marker", "textfont", "error_y", "error_x")) {
        if (!has_attr(type, attr)) next
        if (is_colorway && "textfont" == attr) next
        col <- if (isSingular) rgba_ else if (array_ok(attrs[[attr]]$color)) rgba else NA
        if (is.na(col)) {
          warning("`", attr, ".color` does not currently support multiple values.", call. = FALSE)
        } else {
          trace[[attr]] <- modify_list(list(color = default_(col)), trace[[attr]])
        }
      }
      
      # If trace has fill, `color` controls fillcolor; otherwise line.color
      if (has_fill(trace)) {
        if (!isSingular) warning("Only one fillcolor per trace allowed", call. = FALSE)
        # alpha defaults to 0.5 when applied to fillcolor
        if (is.null(trace[["alpha"]])) rgba_ <- toRGB(rgba_, 0.5)
        if (isSingular) trace <- modify_list(list(fillcolor = default_(rgba_)), trace)
      } else if (has_attr(type, "line")) {
        # if fill does not exist, 'color' controls line.color
        col <- if (isSingular) rgba_ else if (array_ok(attrs$line$color)) rgba else NA
        if (is.na(col)) {
          warning("`line.color` does not currently support multiple values.", call. = FALSE)
        } else {
          trace[["line"]] <- modify_list(list(color = default_(col)), trace[["line"]])
        }
      }
      trace
    }
  )
  
  # i.e., interpret values as color codes
  if (any(isConstant)) {
    colorCodes <- Map(`%||%`, color, rep(colorway, length.out = length(traces)))
    colorCodes <- Map(toRGB, colorCodes[isConstant], alphas[isConstant])
    isColorway <- lengths(color[isConstant]) == 0
    traces[isConstant] <- Map(mapColor, traces[isConstant], colorCodes, isColorway)
  }
  
  # since stroke inherits from color, it should inherit the palette too
  palette <- if (stroke) traces[[1]][["strokes"]] %||% traces[[1]][["colors"]] else traces[[1]][["colors"]]
  
  if (any(isDiscrete)) {
    # unlist() does _not_ preserve order factors
    isOrdered <- all(vapply(color[isDiscrete], is.ordered, logical(1)))
    lvls <- getLevels(unlist(color[isDiscrete]))
    N <- length(lvls)
    pal <- palette %||% if (isOrdered) viridisLite::viridis(N) else RColorBrewer::brewer.pal(N, "Set2")
    colScale <- scales::col_factor(pal, levels = names(pal) %||% lvls, na.color = na.color)
    color_codes <- Map(function(x, y) toRGB(colScale(as.character(x)), y), color[isDiscrete], alphas[isDiscrete])
    traces[isDiscrete] <- Map(mapColor, traces[isDiscrete], color_codes)
  }
  
  if (any(isNumeric)) {
    pal <- palette %||% viridisLite::viridis(10)
    # TODO: use ggstat::frange() when it's on CRAN?
    allColor <- unlist(color[isNumeric])
    rng <- range(allColor, na.rm = TRUE)
    colScale <- scales::col_numeric(pal, rng, na.color = na.color)
    # generate the colorscale to be shared across traces
    vals <- if (diff(rng) > 0) {
      seq(rng[1], rng[2], length.out = 25)
    } else {
      c(0, 1)
    }
    colorScale <- matrix(
      c(scales::rescale(vals), toRGB(colScale(vals), alphas[[1]])),
      ncol = 2
    )
    colorObj <- list(
      colorbar = lapply(list(title = as.character(title), ticklen = 2), default),
      cmin = default(rng[1]),
      cmax = default(rng[2]),
      colorscale = default(colorScale),
      showscale = default(FALSE)
    )
    for (i in which(isNumeric)) {
      # when colorscale is being attached to `z`, we don't need color values in
      # colorObj, so create colorbar trace now and exit early
      if (hasZ[[i]]) {
        colorObj[c("cmin", "cmax")] <- NULL
        colorObj[["showscale"]] <- default(TRUE)
        traces[[i]] <- modify_list(colorObj, traces[[i]])
        traces[[i]]$colorscale <- as_df(traces[[i]]$colorscale)
        # sigh, contour colorscale doesn't support alpha
        if (grepl("contour", traces[[i]][["type"]])) {
          traces[[i]]$colorscale[, 2] <- strip_alpha(traces[[i]]$colorscale[, 2])
        }
        traces[[i]] <- structure(traces[[i]], class = c("plotly_colorbar", "zcolor"))
        next
      }
      
      # if trace is singular (i.e., one unique color in this trace), then there 
      # is no need for a colorscale, and both stroke/color have relevancy
      if (isSingular[[i]]) {
        col <- colScale(uniq(color[[i]]))
        traces[[i]] <- mapColor(traces[[i]], toRGB(col, alphas[[i]]))
      } else {
        
        colorObj$color <- default(color[[i]])
        
        if (stroke) {
          traces[[i]]$marker$line <- modify_list(colorObj, traces[[i]]$marker$line)
        } else {
          traces[[i]]$marker <- modify_list(colorObj, traces[[i]]$marker)
        }
        
        if (hasLine[[i]]) {
          if (hasLineColor[[i]]) {
            traces[[i]]$line <- modify_list(colorObj, traces[[i]]$line)
          } else {
            warning("line.color doesn't (yet) support data arrays", call. = FALSE)
          }
        }
        
        if (hasText[[i]]) {
          if (hasTextColor[[i]]) {
            traces[[i]]$textfont <- modify_list(colorObj, traces[[i]]$textfont)
          } else {
            warning("textfont.color doesn't (yet) support data arrays", call. = FALSE)
          }
        }
        
        # TODO: how to make the summary stat (mean) customizable?
        if (has_fill(traces[[i]])) {
          warning("Only one fillcolor per trace allowed", call. = FALSE)
          col <- toRGB(colScale(mean(colorObj$color, na.rm = TRUE)), alphas[[i]])
          if (is.null(traces[[i]][["alpha"]])) col <- toRGB(col, 0.5)
          traces[[i]] <- modify_list(list(fillcolor = col), traces[[i]])
        }
        
        # make sure the colorscale is going to convert to JSON nicely
        traces[[i]]$marker$colorscale <- as_df(traces[[i]]$marker$colorscale)
      }
    }
  
    # exit early if no additional colorbar trace is needed
    if (any(hasZ)) return(traces)
    if (stroke && sum(lengths(lapply(traces, "[[", "stroke"))) == 0) return(traces)
    
    # add an "empty" trace with the colorbar
    colorObj$color <- rng
    colorObj$showscale <- default(TRUE)
    colorBarTrace <- list(
      x = range(unlist(lapply(traces, "[[", "x")), na.rm = TRUE),
      y = range(unlist(lapply(traces, "[[", "y")), na.rm = TRUE),
      type = if (any(types %in% glTypes())) "scattergl" else "scatter",
      mode = "markers",
      opacity = 0,
      hoverinfo = "none",
      showlegend = FALSE,
      marker = colorObj
    )
    # 3D needs a z property
    if ("scatter3d" %in% types) {
      colorBarTrace$type <- "scatter3d"
      colorBarTrace$z <- range(unlist(lapply(traces, "[[", "z")), na.rm = TRUE)
    }
    if (length(type <- intersect(c("scattergeo", "scattermapbox"), types))) {
      colorBarTrace$type <- type
      colorBarTrace$lat <- colorBarTrace$y
      colorBarTrace$lon <- colorBarTrace$x
      colorBarTrace[["x"]] <- NULL
      colorBarTrace[["y"]] <- NULL
    }
    traces[[length(traces) + 1]] <- structure(colorBarTrace, class = "plotly_colorbar")
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
      list(symbol = default(symbols)), traces[[i]][["marker"]]
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
  if (all(nLinetypes == 0)) return(traces)
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
      list(dash = default(dashes)), traces[[i]][["line"]]
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
  lvls <- lvls[lvls %in% x]
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
    new_dat[[j]]$name <- new_dat[[j]]$name %||% lvls[j]
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


# helper functions
array_ok <- function(attr) isTRUE(tryNULL(attr$arrayOk))
has_fill <- function(trace) {
  trace_type <- trace[["type"]] %||% "scatter"
  # if trace type has fillcolor, but no fill attribute, then fill is always relevant
  has_fillcolor <- has_attr(trace_type, "fillcolor")
  has_fill <- has_attr(trace_type, "fill")
  if (has_fillcolor && !has_fill) return(TRUE)
  fill <- trace[["fill"]] %||% "none"
  if (has_fillcolor && isTRUE(fill != "none")) return(TRUE)
  FALSE
}
