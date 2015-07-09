is.plotly <- function(x) inherits(x, "plotly")

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

#' Evaluate unevaluated arguments for a plotly object 
plotly_build <- function(l) {
  # assume unnamed list elements are data/traces
  nms <- names(l)
  idx <- nms %in% ""
  l <- if (is.null(nms)) {
    list(data = l) 
  } else if (any(idx)) {
    c(data = c(l$data, l[idx]), l[!idx])
  } else l
  dats <- list()
  for (i in seq_along(l$data)) {
    d <- l$data[[i]]
    # if appropriate, evaluate trace arguments in a suitable environment
    idx <- names(d) %in% c("args", "env")
    if (sum(idx) == 2) {
      dat <- c(d[!idx], eval(d$args, as.list(d$env), d$enclos))
      dat[c("args", "env", "enclos")] <- NULL
    } else {
      dat <- d
    }
    # process specially named arguments
    has_color <- !is.null(dat[["color"]]) || !is.null(dat[["z"]])
    has_symbol <- !is.null(dat[["symbol"]])
    has_group <- !is.null(dat[["group"]])
    if (has_color) dats <- c(dats, colorize(dat, as.list(d$args)[["color"]]))
    # TODO: add a legend title (is this only possible via annotations?!?)
    if (has_symbol) dats <- c(dats, symbolize(dat))
    if (has_group) dats <- c(dats, traceify(dat, "group"))
    if (!has_color && !has_symbol && !has_group) dats <- c(dats, list(dat))
  }
  x <- list(data = dats)
  # carry over properties/data from first trace (if appropriate)
  if (length(x$data) > 1 && isTRUE(l$data[[1]]$inherit)) {
    for (i in seq.int(2, length(x$data))) {
      x$data[[i]] <- modifyList(x$data[[1]], x$data[[i]])
    }
  }
  # plot_ly()/layout() may produce a unnamed list of layouts
  # in that case, we may want to evaluate layout arguments
  idx <- names(l$layout) == ""
  if (all(idx)) {
    nlayouts <- length(l$layout)
    layouts <- setNames(vector("list", nlayouts), names(l$layout))
    for (i in seq_len(nlayouts)) {
      layout <- l$layout[[i]]
      idx <- names(layout) %in% c("args", "env")
      layouts[[i]] <- if (sum(idx) == 2) {
        c(layout[!idx], eval(layout$args, as.list(layout$env), layout$enclos)) 
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
  # if style is not null, use it to modify existing traces
  if (!is.null(l$style)) {
    for (i in seq_along(l$style)) {
      sty <- l$style[[i]]
      idx <- names(sty) %in% c("args", "env")
      new_sty <- if (sum(idx) == 2) c(sty[!idx], eval(sty$args, as.list(sty$env), sty$enclos)) else sty
      for (k in sty$traces) x$data[[k]] <- modifyList(x$data[[k]], new_sty)
    }
  }
  # add appropriate axis title (if they don't already exist)
  x <- axis_titles(x, l)
  # create a new plotly if no url is attached to this environment
  x$fileopt <- if (is.null(l$url)) "new" else "overwrite"
  x
}

# returns a _list of traces_.
colorize <- function(dat, title = "") {
  cols <- dat[["color"]] %||% dat[["z"]]
  if (is.numeric(cols)) {
    # by default, match ggplot2 color gradient -- http://docs.ggplot2.org/current/scale_gradient.html
    colors <- dat[["colors"]] %||% c("#132B43", "#56B1F7")
    colz <- scales::col_numeric(colors, cols, na.color = "transparent")(cols)
    df <- if (length(cols) > 1) data.frame(scales::rescale(cols), colz) 
      else data.frame(c(0, 1), rep(colz, 2))
    col_list <- list(
      colorbar = list(title = as.character(title)),
      colorscale = setNames(df, NULL),
      autocolorscale = FALSE,
      color = cols,
      cmin = min(cols),
      cmax = max(cols)
    )
    if (grepl("scatter", dat[["type"]] %||% "scatter")) {
      dat[["marker"]] <- modifyList(col_list, dat[["marker"]] %||% list())
    } else {
      dat <- c(dat, col_list)
    }
    dat <- list(dat)
  } else { # discrete color scale
    dat <- traceify(dat, "color")
    lvls <- unlist(lapply(dat, function(x) unique(x[["color"]])))
    colors <- dat[[1]][["colors"]] %||% 
      RColorBrewer::brewer.pal(length(lvls), if (is.ordered(cols)) "Greens" else "Set2")
    colz <- scales::col_factor(colors, levels = lvls, na.color = "transparent")(lvls)
    dat <- Map(function(x, y) { x[["marker"]] <- c(x[["marker"]], list(color = y)); x }, 
               dat, colz)
  }
  dat <- lapply(dat, function(x) { x$color <- NULL; x$colors <- NULL; x })
  dat
}

symbolize <- function(dat) {
  # symbols really only make sense when markers are in the mode, right?
  dat$mode <- dat$mode %||% "markers"
  dat <- traceify(dat, "symbol")
  dat <- lapply(dat, function(x) { x$symbol <- NULL; x })
  N <- length(dat)
  if (N > 8) warning("Plotly supports 8 different symbols, but you have ", N, " levels!")
  symbols <- c('dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up')
  sym <- symbols[seq_len(N)]
  dat <- Map(function(x, y) { x$marker$symbol <- y; x }, dat, sym)
  dat
}

# break up a single trace into multiple traces according to values stored 
# a particular key name
traceify <- function(dat, nm = "group") {
  x <- dat[[nm]]
  if (is.null(x)) {
    return(list(dat))
  } else {
    # the order of lvls determines the order in which traces are drawn
    # for ordered factors at least, it makes sense to draw the highest level first
    # since that _should_ be the darkest color in a sequential pallette
    lvls <- if (is.factor(x)) rev(levels(x)) else unique(x)
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
}

axis_titles <- function(x, l) {
  for (i in c("x", "y", "z")) {
    s <- lapply(x$data, "[[", i)
    ax <- paste0(i, "axis")
    t <- x$layout[[ax]]$title
    if (is.null(t)) { # deparse the unevaluated expression from 1st trace
      argz <- as.list(l$data[[1]]$args)
      idx <- names(argz) %in% i
      if (any(idx)) x$layout[[ax]]$title <- deparse(argz[idx][[1]])
    }
  }
  x
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
