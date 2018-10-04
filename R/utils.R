is.plotly <- function(x) {
  inherits(x, "plotly")
}

is.formula <- function(f) {
  inherits(f, "formula")
}

is.colorbar <- function(tr) {
  inherits(tr, "plotly_colorbar")
}

is.evaled <- function(p) {
  all(vapply(p$x$attrs, function(attr) inherits(attr, "plotly_eval"), logical(1)))
}

is.webgl <- function(p) {
  if (!is.evaled(p)) p <- plotly_build(p)
  types <- vapply(p$x$data, function(tr) tr[["type"]] %||% "scatter", character(1))
  any(types %in% glTypes())
}

glTypes <- function() {
  c(
    "scattergl", "scatter3d", "mesh3d", "heatmapgl", "pointcloud", "parcoords",
    "surface"
  )
}

# just like ggplot2:::is.discrete()
is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

"%||%" <- function(x, y) {
  if (length(x) > 0 || is_blank(x)) x else y
}

# kind of like %||%, but only respects user-defined defaults
# (instead of defaults provided in the build step)
"%|D|%" <- function(x, y) {
  if (!is.default(x)) x %||% y else y
}

# standard way to specify a line break
br <- function() "<br />"

is.default <- function(x) {
  inherits(x, "plotly_default")
}

default <- function(x) {
  prefix_class(x %||% list(), "plotly_default")
}

compact <- function(x) {
  Filter(Negate(is.null), x)
}

modify_list <- function(x, y, ...) {
  modifyList(x %||% list(), y %||% list(), ...)
}

# convert a vector of dates/date-times to milliseconds
to_milliseconds <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x) * 86400000)
  if (inherits(x, "POSIXt")) return(as.numeric(x) * 1000)
  # throw warning?
  x
}

# apply a function to x, retaining class and "special" plotly attributes
retain <- function(x, f = identity) {
  y <- structure(f(x), class = oldClass(x))
  attrs <- attributes(x)
  # TODO: do we set any other "special" attributes internally 
  # (grepping "structure(" suggests no)
  attrs <- attrs[names(attrs) %in% "apiSrc"]
  if (length(attrs)) {
    attributes(y) <- attrs
  }
  y
}

deparse2 <- function(x) {
  if (is.null(x) || !is.language(x)) return(NULL)
  sub("^~", "", paste(deparse(x, 500L), collapse = ""))
}

new_id <- function() {
  basename(tempfile(""))
}

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

getLevels <- function(x) {
  if (is.factor(x)) levels(x) else sort(unique(x))
}

tryNULL <- function(expr) tryCatch(expr, error = function(e) NULL)

# Don't attempt to do "tidy" data training on these trace types
is_tidy <- function(trace) {
  type <- trace[["type"]] %||% "scatter"
  !type %in% c(
    "mesh3d", "heatmap", "histogram2d", 
    "histogram2dcontour", "contour", "surface"
  )
}

# is grouping relevant for this geometry? (e.g., grouping doesn't effect a scatterplot)
has_group <- function(trace) {
  inherits(trace, paste0("plotly_", c("segment", "path", "line", "polygon"))) ||
    (grepl("scatter", trace[["type"]]) && grepl("lines", trace[["mode"]]))
}

# currently implemented non-positional scales in plot_ly()
npscales <- function() {
  c("color", "stroke", "symbol", "linetype", "size", "span", "split")
}

colorway <- function(p = NULL) {
  colway <- p$x$layout$colorway %||% Schema$layout$layoutAttributes$colorway$dflt
  lapply(as.list(colway), function(x) structure(x, class = "colorway"))
}

# column name for crosstalk key
# TODO: make this more unique?
crosstalk_key <- function() ".crossTalkKey"

# modifyList turns elements that are data.frames into lists
# which changes the behavior of toJSON
as_df <- function(x) {
  if (is.null(x) || is.matrix(x)) return(x)
  if (is.list(x) && !is.data.frame(x)) {
    setNames(as.data.frame(x), NULL)
  }
}

# arrange data if the vars exist, don't throw error if they don't
arrange_safe <- function(data, vars) {
  vars <- vars[vars %in% names(data)]
  if (length(vars)) dplyr::arrange_(data, .dots = vars) else data
}

is_mapbox <- function(p) {
  identical(p$x$layout[["mapType"]], "mapbox")
}

is_geo <- function(p) {
  identical(p$x$layout[["mapType"]], "geo")
}

is_type <- function(p, type) {
  types <- vapply(p$x$data, function(tr) tr[["type"]] %||% "scatter", character(1))
  all(types %in% type)
}

# Replace elements of a nested list
# 
# @param x a named list
# @param indicies a vector of indices. 
# A 1D list may be used to specify both numeric and non-numeric inidices
# @param val the value used to 
# @examples 
# 
# x <- list(a = 1)
# # equivalent to `x$a <- 2`
# re_place(x, "a", 2)
# 
# y <- list(a = list(list(b = 2)))
# 
# # equivalent to `y$a[[1]]$b <- 2`
# y <- re_place(y, list("a", 1, "b"), 3)
# y

re_place <- function(x, indicies = 1, val) {
  
  expr <- call("[[", quote(x), indicies[[1]])
  if (length(indicies) == 1) {
    eval(call("<-", expr, val))
    return(x)
  }
  
  for (i in seq(2, length(indicies))) {
    expr <- call("[[", expr, indicies[[i]])
  }
  
  eval(call("<-", expr, val))
  x
}


# retrive mapbox token if one is set; otherwise, throw error
mapbox_token <- function() {
  token <- Sys.getenv("MAPBOX_TOKEN", NA)
  if (is.na(token)) {
    stop(
      "No mapbox access token found. Obtain a token here\n",
      "https://www.mapbox.com/help/create-api-access-token/\n",
      "Once you have a token, assign it to an environment variable \n",
      "named 'MAPBOX_TOKEN', for example,\n",
      "Sys.setenv('MAPBOX_TOKEN' = 'secret token')", call. = FALSE
    )
  }
  token
}

fit_bounds <- function(p) {
  # Compute layout.mapboxid._fitBounds, an internal attr that has special client-side logic
  # PS. how the hell does mapbox not have a way to set initial map bounds?
  # https://github.com/mapbox/mapbox-gl-js/issues/1970
  mapboxIDs <- grep("^mapbox", sapply(p$x$data, "[[", "subplot"), value = TRUE)
  for (id in mapboxIDs) {
    bboxes <- lapply(p$x$data, function(tr) if (identical(id, tr$subplot)) tr[["_bbox"]])
    rng <- bboxes2range(bboxes, f = 0.01)
    if (!length(rng)) next
    # intentionally an array of numbers in [west, south, east, north] order
    # https://www.mapbox.com/mapbox-gl-js/api/#lnglatboundslike
    p$x$layout[[id]]$`_fitBounds` <- list(
      bounds = c(
        min(rng$xrng),
        min(rng$yrng),
        max(rng$xrng),
        max(rng$yrng)
      ),
      options = list(
        padding = 10, 
        linear = FALSE,
        # NOTE TO SELF: can do something like this to customize easing
        # easing = htmlwidgets::JS("function(x) { return 1; }"),
        offset = c(0, 0)
      )
    )
    p$x$layout[[id]]$center$lat <- mean(rng$yrng)
    p$x$layout[[id]]$center$lon <- mean(rng$xrng)
  }
  
  # Compute layout.geoid.lonaxis.range & layout.geoid.lataxis.range
  # for scattergeo
  geoIDs <- grep("^geo", sapply(p$x$data, "[[", "geo"), value = TRUE)
  for (id in geoIDs) {
    bboxes <- lapply(p$x$data, function(tr) if (identical(id, tr$geo)) tr[["_bbox"]])
    rng <- bboxes2range(bboxes, f = 0.01)
    if (!length(rng)) next
    p$x$layout[[id]]$lataxis$range <- rng$yrng
    p$x$layout[[id]]$lonaxis$range <- rng$xrng
  }
  
  # Compute layout.axisid.scaleanchor & layout.axisid.scaleratio
  # for scatter/scattergl
  rows <- compact(lapply(p$x$data, function(x) c(x[["xaxis"]], x[["yaxis"]])))
  for (i in seq_along(rows)) {
    xid <- rows[[i]][[1]]
    yid <- rows[[i]][[2]]
    bboxes <- lapply(p$x$data, function(tr) {
      if (identical(xid, tr$xaxis) && identical(yid, tr$yaxis)) tr[["_bbox"]]
    })
    rng <- bboxes2range(bboxes, f = 0.01)
    if (!length(rng)) next
    xname <- sub("x", "xaxis", xid)
    yname <- sub("y", "yaxis", yid)
    # default to empty axes
    # TODO: is there a set of projections where it makes sense to show a cartesian grid?
    eaxis <- list(showgrid = FALSE, zeroline = FALSE, ticks = "", showticklabels = FALSE)
    p$x$layout[[xname]] <- modify_list(eaxis, p$x$layout[[xname]])
    p$x$layout[[yname]] <- modify_list(eaxis, p$x$layout[[yname]])
    # remove default axis titles
    p$x$layout[[xname]]$title <- p$x$layout[[xname]]$title %|D|% NULL
    p$x$layout[[yname]]$title <- p$x$layout[[yname]]$title %|D|% NULL
    p$x$layout[[xname]]$scaleanchor <- yid
    # TODO: only do this for lat/lon dat
    p$x$layout[[xname]]$scaleratio <- cos(mean(rng$yrng) * pi/180)
  }
  
  # Internal _bbox field no longer needed
  #p$x$data <- lapply(p$x$data, function(tr) { tr[["_bbox"]] <- NULL; tr })
  p
}

# find the x/y layout range of a collection of trace._bboxes
bboxes2range <- function(bboxes, ...) {
  if (sum(lengths(bboxes)) == 0) return(NULL)
  yrng <- c(
    min(unlist(lapply(bboxes, "[[", "ymin")), na.rm = TRUE),
    max(unlist(lapply(bboxes, "[[", "ymax")), na.rm = TRUE)
  )
  xrng <- c(
    min(unlist(lapply(bboxes, "[[", "xmin")), na.rm = TRUE),
    max(unlist(lapply(bboxes, "[[", "xmax")), na.rm = TRUE)
  )
  list(
    yrng = grDevices::extendrange(yrng, ...),
    xrng = grDevices::extendrange(xrng, ...)
  )
}

# rename attrs (unevaluated arguments) from geo locations (lat/lon) to cartesian
geo2cartesian <- function(p) {
  p$x$attrs <- lapply(p$x$attrs, function(tr) {
    tr[["x"]] <- tr[["x"]] %||% tr[["lat"]]
    tr[["y"]] <- tr[["y"]] %||% tr[["lon"]]
    tr
  })
  p
}

cartesian2geo <- function(p) {
  p$x$data <- lapply(p$x$data, function(tr) {
    if (isTRUE(tr[["type"]] %in% c("scattermapbox", "scattergeo"))) {
      tr[["lat"]] <- tr[["lat"]] %||% tr[["y"]]
      tr[["lon"]] <- tr[["lon"]] %||% tr[["x"]]
      tr[c("x", "y")] <- NULL
    }
    tr
  })
  p
}


is_subplot <- function(p) {
  isTRUE(p$x$subplot)
}

supply_defaults <- function(p) {
  # no need to supply defaults for subplots
  if (is_subplot(p)) return(p)
  # supply trace anchor defaults
  anchors <- if (is_geo(p)) c("geo" = "geo") else if (is_mapbox(p)) c("subplot" = "mapbox") else c("xaxis" = "x", "yaxis" = "y")
  
  p$x$data <- lapply(p$x$data, function(tr) {
    for (i in seq_along(anchors)) {
      key <- names(anchors)[[i]]
      if (!has_attr(tr[["type"]] %||% "scatter", key)) next
      tr[[key]] <- sub("^y1$", "y", sub("^x1$", "x", tr[[key]][1])) %||% anchors[[i]]
    }
    tr
  })
  # hack to avoid https://github.com/ropensci/plotly/issues/945
  if (is_type(p, "parcoords")) p$x$layout$margin$t <- NULL
  
  # supply domain defaults
  geoDomain <- list(x = c(0, 1), y = c(0, 1))
  if (is_geo(p) || is_mapbox(p)) {
    p$x$layout[grepl("^[x-y]axis", names(p$x$layout))] <- NULL
    p$x$layout[[p$x$layout$mapType]] <- modify_list(
      list(domain = geoDomain), p$x$layout[[p$x$layout$mapType]]
    )
  } else {
    types <- vapply(p$x$data, function(tr) tr[["type"]] %||% "scatter", character(1))
    axes <- unlist(lapply(types, function(x) {
      grep("^[a-z]axis$", names(Schema$traces[[x]]$attributes), value = TRUE) %||% NULL
    }))
    for (axis in axes) {
      p$x$layout[[axis]] <- modify_list(
        list(domain = c(0, 1), automargin = TRUE), p$x$layout[[axis]]
      )
    }
  }
  p
}

supply_highlight_attrs <- function(p) {
  # set "global" options via crosstalk variable
  p$x$highlight <- p$x$highlight %||% highlight_defaults()
  
  # defaults are now populated, allowing us to populate some other 
  # attributes such as the selectize widget definition
  sets <- unlist(lapply(p$x$data, "[[", "set"))
  keys <- setNames(lapply(p$x$data, "[[", "key"), sets)
  p$x$highlight$ctGroups <- i(unique(sets))
  
  # TODO: throw warning if we don't detect valid keys?
  hasKeys <- FALSE
  for (i in p$x$highlight$ctGroups) {
    k <- unique(unlist(keys[names(keys) %in% i], use.names = FALSE))
    if (is.null(k)) next
    k <- k[!is.null(k)]
    hasKeys <- TRUE

    # include one selectize dropdown per "valid" SharedData layer
    if (isTRUE(p$x$highlight$selectize)) {
      p$x$selectize[[new_id()]] <- list(
        items = data.frame(value = k, label = k), group = i
      )
    }
    
    # set default values via crosstalk api
    vals <- p$x$highlight$defaultValues[p$x$highlight$defaultValues %in% k]
    if (length(vals)) {
      p <- htmlwidgets::onRender(
        p, sprintf(
          "function(el, x) { crosstalk.group('%s').var('selection').set(%s) }", 
          i, jsonlite::toJSON(as.character(vals), auto_unbox = FALSE)
        )
      )
    }
  }

  # add HTML dependencies, set a sensible dragmode default, & throw messages
  if (hasKeys) {
    p$x$layout$dragmode <- p$x$layout$dragmode %|D|% 
      default(switch(p$x$highlight$on %||% "", plotly_selected = "select") %||% "zoom")
    if (is.default(p$x$highlight$off)) {
      message(
        sprintf(
          "Setting the `off` event (i.e., '%s') to match the `on` event (i.e., '%s'). You can change this default via the `highlight()` function.",
          p$x$highlight$off, p$x$highlight$on
        )
      )
    }
  }
  
  p
}


# make sure plot attributes adhere to the plotly.js schema
verify_attr_names <- function(p) {
  # some layout attributes (e.g., [x-y]axis can have trailing numbers)
  attrs_name_check(
    sub("[0-9]+$", "", names(p$x$layout)),
    c(names(Schema$layout$layoutAttributes), c("barmode", "bargap", "mapType")),
    "layout"
  )
  for (tr in seq_along(p$x$data)) {
    thisTrace <- p$x$data[[tr]]
    attrSpec <- Schema$traces[[thisTrace$type %||% "scatter"]]$attributes
    # make sure attribute names are valid
    attrs_name_check(
      names(thisTrace), 
      c(names(attrSpec), "key", "set", "frame", "transforms", "_isNestedKey", "_isSimpleKey", "_isGraticule", "_bbox"), 
      thisTrace$type
    )
  }
  invisible(p)
}



# ensure both the layout and trace attributes adhere to the plot schema
verify_attr_spec <- function(p) {
  if (!is.null(p$x$layout)) {
    p$x$layout <- verify_attr(
      p$x$layout, Schema$layout$layoutAttributes
    )
  }
  for (tr in seq_along(p$x$data)) {
    thisTrace <- p$x$data[[tr]]
    validAttrs <- Schema$traces[[thisTrace$type %||% "scatter"]]$attributes
    p$x$data[[tr]] <- verify_attr(thisTrace, validAttrs)
    # prevent these objects from sending null keys
    p$x$data[[tr]][["xaxis"]] <- p$x$data[[tr]][["xaxis"]] %||% NULL
    p$x$data[[tr]][["yaxis"]] <- p$x$data[[tr]][["yaxis"]] %||% NULL
  }
  
  p
}

verify_attr <- function(proposed, schema) {
  for (attr in names(proposed)) {
    attrSchema <- schema[[attr]] %||% schema[[sub("[0-9]+$", "", attr)]]
    # if schema is missing (i.e., this is an un-official attr), move along
    if (is.null(attrSchema)) next
    
    valType <- tryNULL(attrSchema[["valType"]]) %||% ""
    role <- tryNULL(attrSchema[["role"]]) %||% ""
    arrayOK <- tryNULL(attrSchema[["arrayOk"]]) %||% FALSE
    isDataArray <- identical(valType, "data_array")
    
    # where applicable, reduce single valued vectors to a constant 
    # (while preserving attributes)
    if (!isDataArray && !arrayOK && !identical(role, "object")) {
      proposed[[attr]] <- retain(proposed[[attr]], uniq)
    }
    
    # plotly.js should really handle this logic
    if (isTRUE(grepl("fill", proposed[["hoveron"]]))) {
      proposed[["text"]] <- paste(uniq(proposed[["text"]]), collapse = "\n")
    }
    
    # ensure data_arrays of length 1 are boxed up by to_JSON()
    if (isDataArray) {
      proposed[[attr]] <- i(proposed[[attr]])
    }
    
    # tag 'src-able' attributes (needed for api_create())
    isSrcAble <- !is.null(schema[[paste0(attr, "src")]]) && length(proposed[[attr]]) > 1
    if (isDataArray || isSrcAble) {
      proposed[[attr]] <- structure(proposed[[attr]], apiSrc = TRUE)
    }
    
    if (length(proposed[["name"]]) > 0) {
      proposed$name <- uniq(proposed$name)
    }
    
    # if marker.sizemode='area', make sure marker.size is boxed up 
    # (otherwise, when marker.size is a constant, it always sets the diameter!)
    # https://codepen.io/cpsievert/pen/zazXgw
    # https://github.com/plotly/plotly.js/issues/2735
    if ("area" %in% proposed$marker$sizemode) {
      proposed$marker[["size"]] <- i(proposed$marker[["size"]])
    }
    
    # do the same for "sub-attributes"
    if (identical(role, "object") && is.recursive(proposed[[attr]])) {
      proposed[[attr]] <- verify_attr(proposed[[attr]], schema[[attr]])
    }
  }
  
  proposed
}

attrs_name_check <- function(proposedAttrs, validAttrs, type = "scatter") {
  illegalAttrs <- setdiff(proposedAttrs, validAttrs)
  if (length(illegalAttrs)) {
    warning("'", type, "' objects don't have these attributes: '",
            paste(illegalAttrs, collapse = "', '"), "'\n", 
            "Valid attributes include:\n'",
            paste(validAttrs, collapse = "', '"), "'\n", 
            call. = FALSE)
  }
  invisible(proposedAttrs)
}

# make sure trace type is valid
# TODO: add an argument to verify trace properties are valid (https://github.com/ropensci/plotly/issues/540)
verify_type <- function(trace) {
  if (is.null(trace$type)) {
    attrs <- names(trace)
    attrLengths <- lengths(trace)
    trace$type <- if (all(c("x", "y", "z") %in% attrs)) {
      if (all(c("i", "j", "k") %in% attrs)) "mesh3d" else "scatter3d"
    } else if (all(c("x", "y") %in% attrs)) {
      xNumeric <- !is.discrete(trace[["x"]])
      yNumeric <- !is.discrete(trace[["y"]])
      if (xNumeric && yNumeric) {
        if (any(attrLengths) > 15000) "scattergl" else "scatter"
      } else if (xNumeric || yNumeric) {
        "bar" 
      } else "histogram2d"
    } else if ("y" %in% attrs || "x" %in% attrs) {
      "histogram"
    } else if ("z" %in% attrs) {
      "heatmap"
    } else {
      warning("No trace type specified and no positional attributes specified", 
              call. = FALSE)
      "scatter"
    }
    relay_type(trace$type)
  }
  if (!is.character(trace$type) || length(trace$type) != 1) {
    stop("The trace type must be a character vector of length 1.\n", 
         call. = FALSE)
  }
  if (!trace$type %in% names(Schema$traces)) {
    stop("Trace type must be one of the following: \n",
         "'", paste(names(Schema$traces), collapse = "', '"), "'",
         call. = FALSE)
  }
  # if scatter/scatter3d/scattergl, default to a scatterplot
  if (grepl("scatter", trace$type) && is.null(trace$mode)) {
    message(
      "No ", trace$type, " mode specifed:\n",
      "  Setting the mode to markers\n",
      "  Read more about this attribute -> https://plot.ly/r/reference/#scatter-mode"
    )
    trace$mode <- "markers"
  }
  trace
}

relay_type <- function(type) {
  message(
    "No trace type specified:\n", 
    "  Based on info supplied, a '", type, "' trace seems appropriate.\n",
    "  Read more about this trace type -> https://plot.ly/r/reference/#", type
  )
  type
}

# Searches a list for character strings and translates R linebreaks to HTML 
# linebreaks (i.e., '\n' -> '<br />'). JavaScript function definitions created 
# via `htmlwidgets::JS()` are ignored
translate_linebreaks <- function(p) {
  recurse <- function(a) {
    typ <- typeof(a)
    if (typ == "list") {
      # retain the class of list elements 
      # which important for many things, such as colorbars
      a[] <- lapply(a, recurse)
    } else if (typ == "character" && !inherits(a, "JS_EVAL")) {
      attrs <- attributes(a)
      a <- gsub("\n", br(), a, fixed = TRUE)
      attributes(a) <- attrs
    }
    a
  }
  p$x[] <- lapply(p$x, recurse)
  p
}

verify_orientation <- function(trace) {
  xNumeric <- !is.discrete(trace[["x"]]) && !is.null(trace[["x"]] %||% NULL)
  yNumeric <- !is.discrete(trace[["y"]]) && !is.null(trace[["y"]] %||% NULL)
  if (xNumeric && !yNumeric) {
    if (any(c("bar", "box") %in% trace[["type"]])) {
      trace$orientation <- "h"
    }
  }
  if (yNumeric && "histogram" %in% trace[["type"]]) {
    trace$orientation <- "h"
  }
  trace
}

verify_mode <- function(p) {
  for (tr in seq_along(p$x$data)) {
    trace <- p$x$data[[tr]]
    if (grepl("scatter", trace$type %||% "scatter")) {
      if (user_specified(trace$marker) && !grepl("markers", trace$mode %||% "")) {
        message(
          "A marker object has been specified, but markers is not in the mode\n",
          "Adding markers to the mode..."
        )
        p$x$data[[tr]]$mode <- paste0(p$x$data[[tr]]$mode, "+markers")
      }
      if (user_specified(trace$line) && !grepl("lines", trace$mode %||% "")) {
        message(
          "A line object has been specified, but lines is not in the mode\n",
          "Adding lines to the mode..."
        )
        p$x$data[[tr]]$mode <- paste0(p$x$data[[tr]]$mode, "+lines")
      }
      if (user_specified(trace$textfont) && !grepl("text", trace$mode %||% "")) {
        warning(
          "A textfont object has been specified, but text is not in the mode\n",
          "Adding text to the mode..."
        )
        p$x$data[[tr]]$mode <- paste0(p$x$data[[tr]]$mode, "+text")
      }
    }
  }
  p
}

# if an object (e.g. trace.marker) contains a non-default attribute, it has been user-specified
user_specified <- function(obj = NULL) {
  if (!length(obj)) return(FALSE)
  !all(rapply(obj, is.default))
}

# populate categorical axes using categoryorder="array" & categoryarray=[]
populate_categorical_axes <- function(p) {
  axes <- p$x$layout[grepl("^xaxis|^yaxis", names(p$x$layout))] %||%
    list(xaxis = NULL, yaxis = NULL)
  for (i in seq_along(axes)) {
    axis <- axes[[i]]
    axisName <- names(axes)[[i]]
    axisType <- substr(axisName, 0, 1)
    # ggplotly() populates these attributes...don't want to clobber that
    if (!is.null(axis$ticktext) || !is.null(axis$tickvals)) next
    # collect all the data that goes on this axis
    d <- lapply(p$x$data, "[[", axisType)
    isOnThisAxis <- function(tr) {
      is.null(tr[["geo"]]) && sub("axis", "", axisName) %in% 
        (tr[[sub("[0-9]+", "", axisName)]] %||% axisType) &&
        # avoid reordering matrices (see #863)
        !is.matrix(tr[["z"]])
    }
    d <- d[vapply(p$x$data, isOnThisAxis, logical(1))]
    if (length(d) == 0) next
    isDiscrete <- vapply(d, is.discrete, logical(1))
    if (0 < sum(isDiscrete) & sum(isDiscrete) < length(d)) {
      warning(
        "Can't display both discrete & non-discrete data on same axis", 
        call. = FALSE
      )
      next
    }
    if (sum(isDiscrete) == 0) next
    categories <- lapply(d, getLevels)
    categories <- unique(unlist(categories))
    if (any(!vapply(d, is.factor, logical(1)))) categories <- sort(categories)
    p$x$layout[[axisName]]$type <- 
      p$x$layout[[axisName]]$type %||% "category"
    p$x$layout[[axisName]]$categoryorder <- 
      p$x$layout[[axisName]]$categoryorder %||% "array"
    p$x$layout[[axisName]]$categoryarray <- 
      p$x$layout[[axisName]]$categoryarray %||% categories
  }
  p
}

verify_arrays <- function(p) {
  for (i in c("annotations", "shapes", "images")) {
    thing <- p$x$layout[[i]]
    if (is.list(thing) && !is.null(names(thing))) {
      p$x$layout[[i]] <- list(thing)
    }
  }
  p
}

verify_hovermode <- function(p) {
  if (!is.null(p$x$layout$hovermode)) {
    return(p)
  }
  types <- unlist(lapply(p$x$data, function(tr) tr$type %||% "scatter"))
  modes <- unlist(lapply(p$x$data, function(tr) tr$mode %||% "lines"))
  if (any(grepl("markers", modes) & types == "scatter") ||
      any(c("plotly_hover", "plotly_click") %in% p$x$highlight$on)) {
    p$x$layout$hovermode <- "closest"
  }
  p
}

verify_key_type <- function(p) {
  keys <- lapply(p$x$data, "[[", "key")
  for (i in seq_along(keys)) {
    k <- keys[[i]]
    if (is.null(k)) next
    if ("select" %in% p$x$layout$clickmode && "plotly_click" %in% p$x$highlight$on) {
      warning(
        "`layout.clickmode` = 'select' is not designed to work well with ",
        "the R package's linking framework (i.e. crosstalk support).",
        call. = FALSE
      )
    }
    # does it *ever* make sense to have a missing key value?
    uk <- uniq(k)
    if (length(uk) == 1) {
      # i.e., the key for this trace has one value. In this case, 
      # we don't have iterate through the entire key, so instead, 
      # we provide a flag to inform client side logic to match the _entire_
      # trace if this one key value is a match
      p$x$data[[i]]$key <- uk[[1]]
      p$x$data[[i]]$`_isSimpleKey` <- TRUE
      p$x$data[[i]]$`_isNestedKey` <- FALSE
    }
    p$x$data[[i]]$`_isNestedKey` <- p$x$data[[i]]$`_isNestedKey` %||% !lazyeval::is_atomic(k)
    # key values should always be strings
    if (p$x$data[[i]]$`_isNestedKey`) {
      p$x$data[[i]]$key <- lapply(p$x$data[[i]]$key, function(x) I(as.character(x)))
      p$x$data[[i]]$key <- setNames(p$x$data[[i]]$key, NULL)
    } else {
      p$x$data[[i]]$key <- I(as.character(p$x$data[[i]]$key))
    }
  }
  p 
}

verify_webgl <- function(p) {
  # see toWebGL
  if (!isTRUE(p$x$.plotlyWebGl)) {
    return(p)
  }
  types <- sapply(p$x$data, function(x) x[["type"]][1] %||% "scatter")
  idx <- paste0(types, "gl") %in% names(Schema$traces)
  if (any(!idx)) {
    warning(
      "The following traces don't have a WebGL equivalent: ",
      paste(which(!idx), collapse = ", ")
    )
  }
  for (i in which(idx)) {
    p$x$data[[i]]$type <- paste0(p$x$data[[i]]$type, "gl")
  }
  p
}

verify_showlegend <- function(p) {
  # this attribute should be set in hide_legend()
  # it ensures that "legend titles" go away in addition to showlegend = FALSE
  if (isTRUE(p$x$.hideLegend)) {
    ann <- p$x$layout$annotations
    is_title <- vapply(ann, function(x) isTRUE(x$legendTitle), logical(1))
    p$x$layout$annotations <- ann[!is_title]
    p$x$layout$showlegend <- FALSE 
  }
  show <- vapply(p$x$data, function(x) x$showlegend %||% TRUE, logical(1))
  # respect only _user-specified_ defaults 
  isSinglePie <- identical("pie", unlist(lapply(p$x$data, function(tr) tr$type))) 
  p$x$layout$showlegend <- p$x$layout$showlegend %|D|%
    default(sum(show) > 1 || isTRUE(p$x$highlight$showInLegend) || isSinglePie)
  p
}

verify_guides <- function(p) {
  
  # since colorbars are implemented as "invisible" traces, prevent a "trivial" legend
  if (has_colorbar(p) && has_legend(p) && length(p$x$data) <= 2) {
    p$x$layout$showlegend <- default(FALSE)
  }
  
  isVisibleBar <- function(tr) {
    is.colorbar(tr) && (tr$showscale %||% TRUE)
  }
  isBar <- vapply(p$x$data, isVisibleBar, logical(1))
  nGuides <- sum(isBar) + has_legend(p)
  
  if (nGuides > 1) {
    
    # place legend at bottom since its scrolly
    yanchor <- default("top")
    y <- default(1 - ((nGuides - 1) / nGuides))
    p$x$layout$legend$yanchor <- p$x$layout$legend$yanchor %|D|% yanchor
    p$x$layout$legend$y <- p$x$layout$legend[["y"]] %|D|% y
    
    # shrink/position colorbars
    idx <- which(isBar)
    for (i in seq_along(idx)) {
      len     <- default(1 / nGuides)
      lenmode <- default("fraction")
      y       <- default(1 - ((i - 1) / nGuides))
      
      j <- idx[[i]]
      tr <- p$x$data[[j]]
      if (inherits(tr, "zcolor")) {
        p$x$data[[j]]$colorbar$len <- tr$colorbar$len %|D|% len
        p$x$data[[j]]$colorbar$lenmode <- tr$colorbar$lenmode %|D|% lenmode
        p$x$data[[j]]$colorbar$y <- tr$colorbar$y %|D|% y
        p$x$data[[j]]$colorbar$yanchor <- tr$colorbar$yanchor %|D|% yanchor
      } else {
        p$x$data[[j]]$marker$colorbar$len <- tr$marker$colorbar$len %|D|% len
        p$x$data[[j]]$marker$colorbar$lenmode <- tr$marker$colorbar$lenmode %|D|% lenmode
        p$x$data[[j]]$marker$colorbar$y <- tr$marker$colorbar$y %|D|% y
        p$x$data[[j]]$marker$colorbar$yanchor <- tr$marker$colorbar$yanchor %|D|% yanchor
      }
    }
    
  }
  
  p
}

verify_mathjax <- function(p) {
  hasMathjax <- "mathjax" %in% sapply(p$dependencies, "[[", "name")
  if (hasMathjax) return(p)
  
  hasTeX <- any(rapply(p$x, is.TeX))
  if (!hasTeX) return(p)
  
  # TODO: it would be much better to add the dependency here, but
  # htmlwidgets doesn't currently support adding dependencies at print-time!
  warning(
    "Detected the use of `TeX()`, but mathjax has not been specified. ",
    "Try running `config(.Last.value, mathjax = 'cdn')`",
    call. = FALSE
  )
  p
}

verify_scattergl_platform <- function(p) {
  if (!identical(.Platform$OS.type, "windows")) return(p)
  if (!is_rstudio()) return(p)
  
  types <- vapply(p$x$data, function(x) x[["type"]] %||% "scatter", character(1))
  if ("scattergl" %in% types) {
    warning(
      "'scattergl' trace types don't currently render in RStudio on Windows. ",
      "Open in another web browser (IE, Chrome, Firefox, etc).",
      call. = FALSE
    )
  }
  
  p
}

has_marker <- function(types, modes) {
  is_scatter <- grepl("scatter", types)
  ifelse(is_scatter, grepl("marker", modes), has_attr(types, "marker"))
}

has_line <- function(types, modes) {
  is_scatter <- grepl("scatter", types)
  ifelse(is_scatter, grepl("line", modes), has_attr(types, "line"))
}

has_text <- function(types, modes) {
  is_scatter <- grepl("scatter", types)
  ifelse(is_scatter, grepl("text", modes), has_attr(types, "textfont"))
}

has_color_array <- function(types, mode = "marker") {
  vapply(types, function(x) isTRUE(tryNULL(Schema$traces[[x]]$attributes[[mode]]$color$arrayOk)), logical(1))
}

has_attr <- function(types, attr = "marker") {
  if (length(attr) != 1) stop("attr must be of length 1")
  vapply(types, function(x) attr %in% names(Schema$traces[[x]]$attributes), logical(1))
}

has_legend <- function(p) {
  showLegend <- function(tr) {
    tr$showlegend %||% TRUE
  }
  any(vapply(p$x$data, showLegend, logical(1))) && 
    isTRUE(p$x$layout$showlegend %|D|% TRUE)
}

has_colorbar <- function(p) {
  isVisibleBar <- function(tr) {
    is.colorbar(tr) && isTRUE(tr$showscale %||% TRUE)
  }
  any(vapply(p$x$data, isVisibleBar, logical(1)))
}

# is a given trace type 3d?
is3d <- function(type = NULL) {
  type <- type %||% "scatter"
  type %in% c("mesh3d", "scatter3d", "surface")
}

# Check for credentials/configuration and throw warnings where appropriate
verify <- function(what = "username", warn = TRUE) {
  val <- grab(what)
  if (val == "" && warn) {
    switch(what,
           username = warning("You need a plotly username. See help(signup, package = 'plotly')", call. = FALSE),
           api_key = warning("You need an api_key. See help(signup, package = 'plotly')", call. = FALSE))
    warning("Couldn't find ", what, call. = FALSE)
  }
  as.character(val)
}

# Check whether a certain credential/configuration exists.
grab <- function(what = "username") {
  who <- paste0("plotly_", what)
  val <- Sys.getenv(who, "")
  # If the environment variable doesn't exist, try reading hidden files that may
  # have been created using other languages or earlier versions of this package
  if (val == "") {
    PLOTLY_DIR <- file.path(normalizePath("~", mustWork = TRUE), ".plotly")
    CREDENTIALS_FILE <- file.path(PLOTLY_DIR, ".credentials")
    CONFIG_FILE <- file.path(PLOTLY_DIR, ".config")
    # note: try_file can be 'succesful', yet return NULL
    val2 <- try_file(CREDENTIALS_FILE, what)
    val <- if (length(nchar(val2)) == 0) try_file(CONFIG_FILE, what) else val2
    val <- val %||% ""
  }
  # return true if value is non-trivial
  setNames(val, who)
}

# try to grab an object key from a JSON file (returns empty string on error)
try_file <- function(f, what) {
  tryCatch(jsonlite::fromJSON(f)[[what]], error = function(e) NULL)
}

# preferred defaults for toJSON mapping
to_JSON <- function(x, ...) {
  jsonlite::toJSON(x, digits = 50, auto_unbox = TRUE, force = TRUE,
                   null = "null", na = "null", ...)
}

# preferred defaults for toJSON mapping
from_JSON <- function(x, ...) {
  jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, ...)
}

i <- function(x) {
  if (is.null(x)) {
    return(NULL)
  } else if (length(x) == 1) {
    return(I(x))
  } else{
    return(x)
  }
}

rm_asis <- function(x) {
  # jsonlite converts NULL to {} and NA to null (plotly prefers null to {})
  # https://github.com/jeroenooms/jsonlite/issues/29
  if (is.null(x)) return(NA)
  if (is.data.frame(x)) return(x)
  if (is.list(x)) lapply(x, rm_asis) 
  # strip any existing 'AsIs' list elements of their 'AsIs' status.
  # this is necessary since ggplot_build(qplot(1:10, fill = I("red"))) 
  # returns list element with their 'AsIs' class, 
  # which conflicts with our JSON unboxing strategy.
  else if (inherits(x, "AsIs")) class(x) <- setdiff(class(x), "AsIs")
  else x
}


# add a class to an object only if it is new, and keep any existing classes of 
# that object
append_class <- function(x, y) {
  structure(x, class = unique(c(class(x), y)))
}
prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}
replace_class <- function(x, new, old) {
  class(x) <- sub(old, new, class(x))
  x
}
remove_class <- function(x, y) {
  oldClass(x) <- setdiff(oldClass(x), y)
  x
}

# TODO: what are some other common configuration options we want to support??
get_domain <- function(type = "") {
  if (type == "api") {
    # new onprem instances don't have an https://api-thiscompany.plot.ly
    # but https://thiscompany.plot.ly seems to just work in that case...
    Sys.getenv("plotly_api_domain", Sys.getenv("plotly_domain", "https://api.plot.ly"))
  } else {
    Sys.getenv("plotly_domain", "https://plot.ly")
  }
}

# plotly's special keyword arguments in POST body
get_kwargs <- function() {
  c("filename", "fileopt", "style", "traces", "layout", "frames", "world_readable")
}

# "common" POST header fields
api_headers <- function() {
  v <- as.character(packageVersion("plotly"))
  httr::add_headers(
    plotly_version = v,
    `Plotly-Client-Platform` = paste("R", v),
    `Content-Type` = "application/json",
    Accept = "*/*"
  )
}

api_auth <- function() {
  httr::authenticate(
    verify("username"),
    verify("api_key")
  )
}


# try to write environment variables to an .Rprofile
cat_profile <- function(key, value, path = "~") {
  r_profile <- file.path(normalizePath(path, mustWork = TRUE),
                         ".Rprofile")
  snippet <- sprintf('\nSys.setenv("plotly_%s" = "%s")', key, value)
  if (!file.exists(r_profile)) {
    message("Creating", r_profile)
    r_profile_con <- file(r_profile)
  }
  if (file.access(r_profile, 2) != 0) {
    stop("R doesn't have permission to write to this file: ", path, "\n",
         "You should consider putting this in an .Rprofile ", "\n",
         "(or sourcing it when you use plotly): ", snippet)
  }
  if (file.access(r_profile, 4) != 0) {
    stop("R doesn't have permission to read this file: ", path)
  }
  message("Adding plotly_", key, " environment variable to ", r_profile)
  cat(snippet, file = r_profile, append = TRUE)
}


# check that suggested packages are installed
try_library <- function(pkg, fun = NULL) {
  if (system.file(package = pkg) != "") {
    return(invisible())
  }
  stop("Package `", pkg, "` required",  if (!is.null(fun)) paste0(" for `", fun, "`"), ".\n", 
       "Please install and try again.", call. = FALSE)
}

# similar logic to rstudioapi::isAvailable()
is_rstudio <- function() {
  identical(.Platform$GUI, "RStudio")
}
