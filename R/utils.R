# this function is called after the package is loaded
.onLoad <- function(...) {
  usr <- verify("username")
  message("Howdy ", usr, "!")
  key <- verify("api_key")
  invisible(NULL)
}

# Check for credentials/configuration and throw warnings where appropriate
verify <- function(what = "username") {
  val <- grab(what)
  if (val == "") {
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
    val2 <- try_file(CREDENTIALS_FILE, what)
    val <- if (val2 == "") try_file(CONFIG_FILE, what) else val2
  }
  # return true if value is non-trivial
  setNames(val, who)
}

# try to grab an object key from a JSON file (returns empty string on error)
try_file <- function(f, what) {
  tryCatch(jsonlite::fromJSON(f)[[what]], error = function(e) "")
}

# preferred defaults for toJSON mapping
to_JSON <- function(x, ...) {
  jsonlite::toJSON(x, digits = 50, auto_unbox = TRUE, force = TRUE, ...)
}

# preferred defaults for toJSON mapping
from_JSON <- function(x, ...) {
  jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, ...)
}

# added a class to existing class(es) of an object and return the object
struct <- function(x, y) structure(x, class = c(class(x), y))

# TODO: what are some other common configuration options we want to support??
get_domain <- function() {
  Sys.getenv("plotly_domain", "https://plot.ly")
}

# plotly's special keyword arguments in POST body
get_kwargs <- function() {
  c("filename", "fileopt", "style", "traces", "layout", 
    "world_readable", "kwarg_example")
}

# POST header fields
plotly_headers <- function() {
  httr::add_headers(.headers = c(
    "plotly-username" = verify("username"),
    "plotly-apikey" = verify("api_key"),
    "plotly-version" = as.character(packageVersion("plotly")),
    "plotly-platform" = "R"))
}

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
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
  if (file.access(r_profile, 2) != 0)
    stop("R doesn't have permission to write to this file: ", path)
  if (file.access(r_profile, 4) != 0)
    stop("R doesn't have permission to read this file: ", path)
  message("Adding plotly_", key, " environment variable to ", r_profile)
  cat(snippet, file = r_profile, append = TRUE)
}
