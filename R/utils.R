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
  c("scattergl", "scatter3d", "mesh3d", "heatmapgl", "pointcloud", "parcoords")
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
  structure(x, class = "plotly_default")
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
  c("color", "symbol", "linetype", "size", "split")
}

# copied from https://github.com/plotly/plotly.js/blob/master/src/components/color/attributes.js
traceColorDefaults <- function() {
  c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', 
    '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
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

# rename attrs (unevaluated arguments) from geo locations (lat/lon) to cartesian
geo2cartesian <- function(p) {
  p$x$attrs <- lapply(p$x$attrs, function(tr) {
    tr[["x"]] <- tr[["x"]] %||% tr[["lat"]]
    tr[["y"]] <- tr[["y"]] %||% tr[["lon"]]
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
  # supply domain defaults
  geoDomain <- list(x = c(0, 1), y = c(0, 1))
  if (is_geo(p) || is_mapbox(p)) {
    p$x$layout[grepl("^[x-y]axis", names(p$x$layout))] <- NULL
    p$x$layout[[p$x$layout$mapType]] <- modify_list(
      list(domain = geoDomain), p$x$layout[[p$x$layout$mapType]]
    )
  } else {
    axes <- if (is_type(p, "scatterternary"))  {
      c("aaxis", "baxis", "caxis") 
    } else if (is_type(p, "pie") || is_type(p, "parcoords")) {
      NULL
    } else {
      c("xaxis", "yaxis")
    }
    for (axis in axes) {
      p$x$layout[[axis]] <- modify_list(
        list(domain = c(0, 1)), p$x$layout[[axis]]
      )
    }
  }
  p
}

supply_highlight_attrs <- function(p) {
  # set "global" options via crosstalk variable
  hd <- highlight_defaults()
  ctOpts <- Map(function(x, y) getOption(x, y), names(hd), hd)
  p <- htmlwidgets::onRender(
    p, sprintf(
      "function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set(%s); }", 
      jsonlite::toJSON(ctOpts, auto_unbox = TRUE)
    )
  )
  
  # use "global" options as the default, but override with non-default options
  # specified via highlight()
  p$x$highlight <- p$x$highlight %||% hd
  for (opt in names(ctOpts)) {
    isDefault <- identical(p$x$highlight[[opt]], hd[[opt]])
    if (isDefault) p$x$highlight[[opt]] <- ctOpts[[opt]]
  }
  
  # defaults are now populated, allowing us to populate some other 
  # attributes such as the selectize widget definition
  sets <- unlist(lapply(p$x$data, "[[", "set"))
  keys <- setNames(lapply(p$x$data, "[[", "key"), sets)
  p$x$highlight$ctGroups <- I(unique(sets))
  
  # TODO: throw warning if we don't detect valid keys?
  for (i in p$x$highlight$ctGroups) {
    k <- unique(unlist(keys[names(keys) %in% i], use.names = FALSE))
    if (is.null(k)) next
    k <- k[!is.null(k)]
    
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
          i, jsonlite::toJSON(vals, auto_unbox = FALSE)
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
      c(names(attrSpec), "key", "set", "frame", "transforms", "_isNestedKey", "_isSimpleKey"), 
      thisTrace$type
    )
  }
  invisible(p)
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

# ensure both the layout and trace attributes adhere to the plot schema
verify_attr_spec <- function(p) {
  if (!is.null(p$x$layout)) {
    layoutNames <- names(p$x$layout)
    layoutNew <- verify_attr(
      setNames(p$x$layout, sub("[0-9]+$", "", layoutNames)),
      Schema$layout$layoutAttributes
    )
    p$x$layout <- setNames(layoutNew, layoutNames)
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
    attrSchema <- schema[[attr]]
    valType <- tryNULL(attrSchema[["valType"]]) %||% ""
    role <- tryNULL(attrSchema[["role"]]) %||% ""
    # ensure data_arrays of length 1 are boxed up by to_JSON()
    if (identical(valType, "data_array")) {
      proposed[[attr]] <- i(proposed[[attr]])
    }
    # where applicable, reduce single valued vectors to a constant 
    # (while preserving any 'special' attribute class)
    if (!valType %in% c("data_array", "any") && !identical(role, "object")) {
      proposed[[attr]] <- structure(
        uniq(proposed[[attr]]), class = oldClass(proposed[[attr]])
      )
    }
    # do the same for "sub-attributes"
    if (identical(role, "object")) {
      for (attr2 in names(proposed[[attr]])) {
        valType2 <- tryNULL(attrSchema[[attr2]][["valType"]]) %||% ""
        role2 <- tryNULL(attrSchema[[attr2]][["role"]]) %||% ""
        # ensure data_arrays of length 1 are boxed up by to_JSON()
        if (identical(valType2, "data_array")) {
          proposed[[attr]][[attr2]] <- i(proposed[[attr]][[attr2]])
        }
        # where applicable, reduce single valued vectors to a constant
        if (!valType2 %in% c("data_array", "any", "color") && !identical(role2, "object")) {
          proposed[[attr]][[attr2]] <- structure(
            uniq(proposed[[attr]][[attr2]]), class = oldClass(proposed[[attr]][[attr2]])
          )
        }
        # we don't have to go more than two-levels, right?
      }
    }
  }
  proposed
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
      if (!is.null(trace$marker) && !grepl("markers", trace$mode %||% "")) {
        message(
          "A marker object has been specified, but markers is not in the mode\n",
          "Adding markers to the mode..."
        )
        p$x$data[[tr]]$mode <- paste0(p$x$data[[tr]]$mode, "+markers")
      }
      if (!is.null(trace$line) && !grepl("lines", trace$mode %||% "")) {
        message(
          "A line object has been specified, but lines is not in the mode\n",
          "Adding lines to the mode..."
        )
        p$x$data[[tr]]$mode <- paste0(p$x$data[[tr]]$mode, "+lines")
      }
      if (!is.null(trace$textfont) && !grepl("text", trace$mode %||% "")) {
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
  for (i in c("annotations", "shapes")) {
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

verify_dragmode <- function(p) {
  if (!is.null(p$x$layout$dragmode)) {
    return(p)
  }
  selecty <- has_highlight(p) && "plotly_selected" %in% p$x$highlight$on
  p$x$layout$dragmode <- if (selecty) "lasso" else "zoom"
  p
}

verify_key_type <- function(p) {
  keys <- lapply(p$x$data, "[[", "key")
  for (i in seq_along(keys)) {
    k <- keys[[i]]
    if (is.null(k)) next
    uk <- unique(k)
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

has_highlight <- function(p) {
  hasKey <- any(vapply(p$x$data, function(x) length(x[["key"]]), integer(1)) > 1)
  hasKey && !is.null(p[["x"]][["highlight"]])
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
  p$x$layout$showlegend <- p$x$layout$showlegend %|D|%
    default(sum(show) > 1 || isTRUE(p$x$highlight$showInLegend))
  p
}

verify_guides <- function(p) {
  
  # since colorbars are implemented as "invisible" traces, prevent a "trivial" legend
  if (has_colorbar(p) && has_legend(p) && length(p$x$data) <= 2) {
    p$x$layout$showlegend <- default(FALSE)
  }
  
  isVisibleBar <- function(tr) {
    is.colorbar(tr) && isTRUE(tr$showscale %||% TRUE)
  }
  isBar <- vapply(p$x$data, isVisibleBar, logical(1))
  nGuides <- sum(isBar) + has_legend(p)
  
  if (nGuides > 1) {
    
    # place legend at bottom since its scrolly
    p$x$layout$legend <- modify_list(
      list(y = 1 - ((nGuides - 1) / nGuides), yanchor = "top"),
      p$x$layout$legend
    )
    
    idx <- which(isBar)
    for (i in seq_along(idx)) {
      p <- colorbar_built(
        p, which = i, len = 1 / nGuides, y = 1 - ((i - 1) / nGuides), 
        lenmode = "fraction",  yanchor = "top"
      )
    }
    
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

# TODO: what are some other common configuration options we want to support??
get_domain <- function(type = "") {
  if (type == "api") {
    Sys.getenv("plotly_api_domain", "https://api.plot.ly")
  } else {
    Sys.getenv("plotly_domain", "https://plot.ly")
  }
}

# plotly's special keyword arguments in POST body
get_kwargs <- function() {
  c("filename", "fileopt", "style", "traces", "layout", "frames", "world_readable")
}

# POST header fields
#' @importFrom base64enc base64encode
plotly_headers <- function(type = "main") {
  usr <- verify("username")
  key <- verify("api_key")
  v <- as.character(packageVersion("plotly"))
  h <- if (type == "v2") {
    auth <- base64enc::base64encode(charToRaw(paste(usr, key, sep = ":")))
    c(
      "authorization" = paste("Basic", auth),
      "plotly-client-platform" = paste("R", v),
      "plotly_version" = v,
      "content-type" = "application/json"
    )
  } else {
    c(
      "plotly-username" = usr,
      "plotly-apikey" = key,
      "plotly-version" = v,
      "plotly-platform" = "R"
    )
  }
  httr::add_headers(.headers = h)
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
