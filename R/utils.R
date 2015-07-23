is.plotly <- function(x) inherits(x, "plotly")
is.offline <- function(x) inherits(x, "offline")

"%||%" <- function(x, y) {
  if (length(x) > 0) x else y
}

# this function is called after the package is loaded
.onAttach <- function(...) {
  usr <- verify("username")
  if (nchar(usr) > 0) 
    packageStartupMessage("\n", "Howdy, ", usr, "!")
  key <- verify("api_key")
  if (nchar(key) > 0) {
    packageStartupMessage("Sweet, you have an API key already! \n",
                          "Start making plots with ggplotly() or plot_ly().")
  }
  # set a default for the offline bundle directory 
  if (Sys.getenv("plotly_offline") == "") {
    Sys.setenv("plotly_offline" = "~/.plotly/plotlyjs")
    # maybe rely a message if bundle is (or isn't) found?
  }
  invisible(NULL)
}

# special enviroment that tracks trace/layout information
plotlyEnv <- new.env(parent = emptyenv())

# hash plot info, assign it to the special plotly environment, & attach it to data
hash_plot <- function(df, p) {
  if (missing(df) || is.null(df)) df <- data.frame()
  hash <- digest::digest(p)
  assign(hash, p, envir = plotlyEnv)
  attr(df, "plotly_hash") <- hash
  # add plotly class mainly for printing method
  class(df) <- unique(c("plotly", class(df)))
  # return a data frame to be compatible with things like dplyr
  df
}

#' Obtain underlying data of plotly object
#' 
#' Given a data frame with a class of plotly, this function returns the arguments
#' and/or data used to create the plotly. If no data frame is provided, 
#' the last plotly object created in this R session is returned (if it exists).
#' 
#' @param data a data frame with a class of plotly (and a plotly_hash attribute).
get_plot <- function(data = NULL, strict = TRUE) {
  hash <- attr(data, "plotly_hash")
  if (!is.null(hash)) {
    get(hash, envir = plotlyEnv)
  } else if (is.data.frame(data)) {
    # safe to just grab the most recent environment?
    hash <- rev(ls(plotlyEnv))[1]
    plotlyEnv[[hash]]
  } else {
    data
  }
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
  jsonlite::toJSON(x, digits = 50, auto_unbox = TRUE, force = TRUE, ...)
}

# preferred defaults for toJSON mapping
from_JSON <- function(x, ...) {
  jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, ...)
}

# plotlyjs properties that must _always_ be an array (even if length 1)
get_boxed <- function() {
  c("x", "y", "lat", "lon", "text")
}

# add a class to an object only if it is new, and keep any existing classes of 
# that object
struct <- function(x, y, ...) {
  structure(x, class = unique(c(class(x), y)), ...)
} 

# TODO: what are some other common configuration options we want to support??
get_domain <- function(type = "main") {
  if (type == "stream") {
    Sys.getenv("plotly_streaming_domain", "http://stream.plot.ly")
  } else {
    Sys.getenv("plotly_domain", "https://plot.ly")
  }
  
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
