is.plotly <- function(x) {
  inherits(x, "plotly")
}

is.formula <- function(f) {
  inherits(f, "formula")
}

is.colorbar <- function(tr) {
  inherits(tr, "plotly_colorbar")
}

is.bare.list <- function(x) {
  is.list(x) && !is.data.frame(x)
}

"%||%" <- function(x, y) {
  if (length(x) > 0 || is_blank(x)) x else y
}

compact <- function(x) {
  Filter(Negate(is.null), x)
}

modify_list <- function(x, y, ...) {
  modifyList(x %||% list(), y %||% list(), ...)
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
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

# currently implemented non-positional scales in plot_ly()
npscales <- function() {
  c("color", "symbol", "linetype", "size")
}

# modifyList turns elements that are data.frames into lists
# which changes the behavior of toJSON
as_df <- function(x) {
  if (is.null(x) || is.matrix(x)) return(x)
  if (is.list(x) && !is.data.frame(x)) {
    setNames(as.data.frame(x), NULL)
  }
}

# make sure plot attributes adhere to the plotly.js schema
verify_attr_names <- function(p) {
  # some layout attributes (e.g., [x-y]axis can have trailing numbers)
  check_attrs(
    sub("[0-9]+$", "", names(p$x$layout)),
    c(names(Schema$layout$layoutAttributes), c("barmode", "bargap"))
  )
  for (tr in seq_along(p$x$data)) {
    thisTrace <- p$x$data[[tr]]
    validAttrs <- Schema$traces[[thisTrace$type %||% "scatter"]]$attributes
    check_attrs(names(thisTrace), c(names(validAttrs), "key"))
  }
  invisible(p)
}

check_attrs <- function(proposedAttrs, validAttrs) {
  illegalAttrs <- setdiff(proposedAttrs, validAttrs)
  if (length(illegalAttrs)) {
    warning("The following attributes don't exist:\n'",
         paste(illegalAttrs, collapse = "', '"), "'\n", 
         "Valid options include:\n'",
         paste(validAttrs, collapse = "', '"), "'\n", 
         call. = FALSE)
  }
  invisible(proposedAttrs)
}

# ensure both the layout and trace attributes are sent to plotly.js
# as data_arrays
verify_boxed <- function(p) {
  if (!is.null(p$x$layout)) {
    layoutNames <- names(p$x$layout)
    layoutNew <- verify_box(
      setNames(p$x$layout, sub("[0-9]+$", "", layoutNames)),
      Schema$layout$layoutAttributes
    )
    p$x$layout <- setNames(layoutNew, layoutNames)
  }
  for (tr in seq_along(p$x$data)) {
    thisTrace <- p$x$data[[tr]]
    validAttrs <- Schema$traces[[thisTrace$type %||% "scatter"]]$attributes
    p$x$data[[tr]] <- verify_box(thisTrace, validAttrs)
  }
  p
}

verify_box <- function(proposed, schema) {
  for (attr in names(proposed)) {
    attrVal <- proposed[[attr]]
    attrSchema <- schema[[attr]]
    isArray <- tryCatch(
      identical(attrSchema[["valType"]], "data_array"),
      error = function(e) FALSE
    )
    isObject <- tryCatch(
      identical(attrSchema[["role"]], "object"),
      error = function(e) FALSE
    )
    if (isArray) {
      proposed[[attr]] <- i(attrVal)
    }
    # we don't have to go more than two-levels, right?
    if (isObject) {
      for (attr2 in names(attrVal)) {
        isArray2 <- tryCatch(
          identical(attrSchema[[attr2]][["valType"]], "data_array"),
          error = function(e) FALSE
        )
        if (isArray2) {
          proposed[[attr]][[attr2]] <- i(attrVal[[attr2]])
        }
      }
    }
  }
  proposed
}


# make sure trace type is valid
# TODO: add an argument to verify trace properties are valid (https://github.com/ropensci/plotly/issues/540)
verify_type <- function(trace) {
  isNULL <- is.null(trace$type)
  if (isNULL) {
    attrs <- names(trace)
    attrLengths <- lengths(trace)
    if (all(c("x", "y", "z") %in% attrs)) {
      trace$type <- if (all(c("i", "j", "k") %in% attrs)) "mesh3d" else "scatter3d"
    } else if (all(c("x", "y") %in% attrs)) {
      if (!is.discrete(trace$x) && !is.discrete(trace$y)) {
        trace$type <- if (any(attrLengths) > 15000) "scattergl" else "scatter"
      } else if (!is.discrete(trace$x)) {
        trace$type <- "bar"
        trace$orientation <- "h"
      } else if (!is.discrete(trace$y)) {
        trace$type <- "bar"
      } else {
        trace$type <- "histogram2d"
      }
    } else if ("y" %in% attrs || "x" %in% attrs) {
      trace$type <- "histogram"
    } else if ("z" %in% attrs) {
      trace$type <- "heatmap"
    } else {
      warning("No trace type specified and no positional attributes specified", 
              call. = FALSE)
      trace$type <- "scatter"
    }
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
  if (grepl("scatter", trace$type)) {
    trace$mode <- trace$mode %||% "markers"
  }
  if (isNULL) relay_type(trace$type, trace$mode)
  trace
}

relay_type <- function(type, mode = NULL) {
  message(
    "No trace type specified. Applying `add_", mode %||% type, "()`.\n",
    "Read more about this trace type here -> https://plot.ly/r/reference/#", type
  )
  type
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
        (tr[[sub("[0-9]+", "", axisName)]] %||% axisType)
    }
    d <- d[vapply(p$x$data, isOnThisAxis, logical(1))]
    if (length(d) == 0) next
    isDiscrete <- vapply(d, is.discrete, logical(1))
    if (0 < sum(isDiscrete) & sum(isDiscrete) < length(d)) {
      stop("Can't display both discrete & non-discrete data on same axis")
    }
    if (sum(isDiscrete) == 0) next
    categories <- lapply(d, function(x) if (is.factor(x)) levels(x) else unique(x))
    categories <- unique(unlist(categories))
    if (any(!vapply(d, is.factor, logical(1)))) categories <- sort(categories)
    p$x$layout[[axisName]]$type <- "category"
    p$x$layout[[axisName]]$categoryorder <- "array"
    p$x$layout[[axisName]]$categoryarray <- categories
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
  if (any(grepl("markers", modes) & types == "scatter")) {
    p$x$layout$hovermode <- "closest"
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
    isTRUE(p$x$layout$showlegend %||% TRUE)
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
  c("filename", "fileopt", "style", "traces", "layout", "world_readable")
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
