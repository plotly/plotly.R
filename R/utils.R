is.plotly <- function(x) {
  inherits(x, "plotly")
}

is.formula <- function(f) {
  inherits(f, "formula")
}

"%||%" <- function(x, y) {
  if (length(x) > 0 || is_blank(x)) x else y
}

compact <- function(x) {
  Filter(Negate(is.null), x)
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

deparse2 <- function(x) {
  if (is.null(x)) return(NULL)
  paste(deparse(x, 500L), collapse = "")
}

new_id <- function() {
  basename(tempfile(""))
}

verify_arg <- function(arg) {
  if (missing(arg)) return(NULL)
  if (!is.formula(arg)) {
    stop("Argument must be a formula.")
  }
  arg
}

# make sure plot attributes are valid according to the plotly.js schema
verify_plot <- function(p) {
  # some layout attributes (e.g., [x-y]axis can have trailing numbers)
  layoutAttrs <- sub("[0-9]+$", "", names(p$x$layout))
  validLayoutAttrs <- names(Schema$layout$layoutAttributes)
  illegalLayoutAttrs <- setdiff(layoutAttrs, validLayoutAttrs)
  if (length(illegalLayoutAttrs)) {
    stop("The following layout attributes don't exist:\n'",
         paste(illegalLayoutAttrs, collapse = "', '"), "'\n", 
         "Valid layout attributes include:\n'",
         paste(validLayoutNames, collapse = "', '"), "'\n", 
         call. = FALSE)
  }
  
  traceTypes <- unlist(lapply(p$x$data, "[[", "type"))
  validTraceTypes <- names(Schema$traces)
  illegalTraceTypes <- setdiff(traceTypes, validTraceTypes)
  if (length(illegalTraceTypes)) {
    stop("The following trace types don't exist:\n'",
         paste(illegalTraceTypes, collapse = "', '"), "'\n", 
         "Valid layout attributes include:\n'",
         paste(validTraceTypes, collapse = "', '"), "'\n", 
         call. = FALSE)
  }
  invisible(p)
}

# make sure trace type is valid
# TODO: add an argument to verify trace properties are valid (https://github.com/ropensci/plotly/issues/540)
verify_type <- function(type = NULL) {
  if (is.null(type)) {
    message("No trace type specified. Guessing you want a 'scatter' trace")
    type <- "scatter"
  }
  if (!is.character(type) || length(type) != 1) {
    stop("The trace type must be a character vector of length 1.\n", 
         call. = FALSE)
  }
  if (!type %in% names(Schema$traces)) {
    stop("Trace type must be one of the following: \n",
         "'", paste(names(Schema$traces), collapse = "', '"), "'",
         call. = FALSE)
  }
  type
}

# verify_attrs <- function(type = NULL, attributes = NULL) {
#   type <- verify_type(type)
#   attrs <- traces[[type]]$attributes
#   idx <- attributes %in% names(attrs)
#   if (any(!idx)) {
#     stop(
#       "The '", type, "' trace type doesn't have attribute(s) named: '", 
#       paste(attributes[!idx], collapse = "', '"), "'", call. = FALSE
#     )
#   }
#   attrs[attributes]
# }


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

has_attr <- function(types, attr) {
  isMissing <- function(x) {
    e <- try(verify_attrs(x, attr), silent = TRUE)
    inherits(e, "try-error")
  }
  !vapply(types, isMissing, logical(1), USE.NAMES = FALSE)
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
    inherits(tr, "plotly_colorbar") && isTRUE(tr$showscale %||% TRUE)
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

add_boxed <- function(x) {
  for (i in seq_along(x$data)) {
    # some object keys require an array, even if length one
    # one way to ensure atomic vectors of length 1 are not automatically unboxed,
    # by to_JSON(), is to attach a class of AsIs (via I())
    d <- x$data[[i]]
    idx <- names(d) %in% get_boxed(d$type %||% "scatter") & sapply(d, length) == 1
    if (any(idx)) x$data[[i]][idx] <- lapply(d[idx], I)
    # (safely) mark individual nested properties
    x$data[[i]]$error_x$array <- i(d$error_x$array)
    x$data[[i]]$error_y$array <- i(d$error_y$array)
    x$data[[i]]$error_x$arrayminus <- i(d$error_x$arrayminus)
    x$data[[i]]$error_y$arrayminus <- i(d$error_y$arrayminus)
  }
  axes <- grep("^[x-y]axis", names(x$layout))
  for (ax in axes) {
    x$layout[[ax]]$ticktext <- i(x$layout[[ax]]$ticktext)
    x$layout[[ax]]$tickvals <- i(x$layout[[ax]]$tickvals)
  }
  x
}

# plotlyjs properties that must _always_ be an array (even if length 1)
get_boxed <- function(type = "scatter") {
  # if the trace type isn't found, provide some sensible defaults
  boxers[[type]] %||% c("x", "y", "z", "lat", "lon", "text", "locations")
}

# if this ever needs updating see
# https://github.com/ropensci/plotly/issues/415#issuecomment-173353138
boxers <- list(
  choropleth = c("locations", "z", "text"),
  box = c("x", "y"),
  heatmap = c("z", "text"),
  histogram = c("x", "y"),
  histogram2d = c("z", "color"),
  mesh3d = c("x", "y", "z", "i", "j", "k", "intensity", "vertexcolor", "facecolor"),
  # TODO: what to do about marker.colors?
  pie = c("labels", "values", "text"),
  scatter = c("x", "y", "r", "t"),
  scatter3d = c("x", "y", "z"),
  scattergeo = c("lon", "lat", "locations"),
  surface = c("x", "y", "z", "text")
)

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
