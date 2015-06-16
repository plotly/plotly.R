#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

is.plotly <- function(x) inherits(x, "plotly")

# this function is called after the package is loaded
.onLoad <- function(...) {
  usr <- verify("username")
  message("Howdy ", usr, "!")
  key <- verify("api_key")
  invisible(NULL)
}

# special enviroment that tracks trace/layout information
plotlyEnv <- new.env(parent = emptyenv())

# hash plot info, assign it to the special plotly environment, & attach it to data
hash_plot <- function(df, p) {
  hash <- digest::digest(p)
  assign(hash, p, envir = plotlyEnv)
  attr(df, "plotly_hash") <- hash
  # add plotly class mainly for printing method
  class(df) <- unique(c("plotly", class(df)))
  # return a data frame to be compatible with things like dplyr
  df
}

# get plot info given a 
get_plot <- function(data, strict = TRUE) {
  hash <- attr(data, "plotly_hash")
  if (!is.null(hash)) {
    p <- get(hash, envir = plotlyEnv)
  } else {
    # safe to just grab the most recent environment?
    hash <- rev(ls(plotlyEnv))[1]
    p <- plotlyEnv[[hash]]
    if (strict) 
      warning("Output may not be correct since data isn't a plotly object")
  }
  p
}

# evaluate unevaluated expressions before POSTing to plotly
eval_list <- function(l) {
  x <- list()
  ntraces <- length(l$data)
  x$data <- vector("list", ntraces)
  # when appropriate, evaluate trace arguments in a suitable environment
  for (i in seq_len(ntraces)) {
    dat <- l$data[[i]]
    idx <- names(dat) %in% c("args", "env")
    x$data[[i]] <- if (sum(idx) == 2) c(dat[!idx], eval(dat$args, dat$env)) else dat
  }
  # translate colors and shapes
  title <- as.character(as.list(l$data[[1]]$args)[["color"]])
  x$data <- colorize(x$data, title)

  # carry over data from first trace (if appropriate)
  if (ntraces > 1 && isTRUE(l$data[[1]]$inherit)) {
    for (i in seq.int(2, ntraces)) {
      x$data[[i]] <- modifyList(x$data[[1]], x$data[[i]])
    }
  }
  # plot_ly()/layout() will produce a unnamed list of layouts
  # in that case, we may want to evaluate layout arguments
  idx <- names(l$layout) == ""
  if (all(idx)) {
    nlayouts <- length(l$layout)
    layouts <- setNames(vector("list", nlayouts), names(l$layout))
    for (i in seq_len(nlayouts)) {
      layout <- l$layout[[i]]
      idx <- names(layout) %in% c("args", "env")
      layouts[[i]] <- if (sum(idx) == 2) {
        c(layout[!idx], eval(layout$args, layout$env)) 
      } else {
        layout
      }
    }
    idx <- names(layouts) == ""
    x$layout <- if (any(idx)) {
      c(Reduce(c, layouts[idx]), layouts[!idx])
    } else {
      Reduce(c, layouts)
    }
  } else {
    x$layout <- l$layout
  }
  # create a new plotly if no url is attached to this environment
  x$fileopt <- if (is.null(l$url)) "new" else "overwrite"
  x
}


colorize <- function(data, title = "") {
  # TODO: how to provide a way to change default color scale?
  # IDEA: provide some scale_*() functions?
  seq_dat <- seq_along(data)
  for (i in seq_dat) {
    cols <- data[[i]]$color
    if (!is.null(cols)) {
      if (is.numeric(cols)) {
        cols <- unique(scales::rescale(cols))
        o <- order(cols, decreasing = FALSE)
        # match ggplot2 color gradient -- http://docs.ggplot2.org/current/scale_gradient.html
        colz <- scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7")(cols)
        df <- setNames(data.frame(cols[o], colz[o]), NULL)
        data[[i]][["marker"]] <- list(
          colorbar = list(title = title),
          colorscale = df,
          color = data[[i]]$color,
          cmin = min(data[[i]]$color), 
          cmax = max(data[[i]]$color)
        )
      } else { # discrete color scale
        lvls <- if (is.factor(cols)) levels(cols) else unique(cols)
        colz <- scales::col_factor("Set2", domain = lvls)(cols)
        # break up data into multiple traces (so legend appears). We assume
        # any column with same length as color vector should be split
        lens <- lapply(data[[i]], length)
        idx <- lens == length(cols)
        new_dat <- list()
        # TODO
        for (j in seq_along(lvls)) {
          idx2 <- which(cols == lvls[j])
          sub_dat <- as.data.frame(data[[i]][idx])[idx2, ]
          new_dat[[j]] <- c(as.list(sub_dat), data[[i]][!idx])
          new_dat[[j]]$name <- lvls[j]
        }
        # TODO: construct appropriate aux object? get type from trace?
        data <- c(new_dat, data[setdiff(seq_dat, i)])
      }
    }
  }
  data
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

# add a class to an object only if it is new, and keep any existing classes of 
# that object
struct <- function(x, y, ...) {
  structure(x, class = unique(c(class(x), y)), ...)
} 

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
