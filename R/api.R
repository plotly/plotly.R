api_create_plot <- function(x = last_plot(), filename = NULL,
                            sharing = c("public", "private", "secret"), ...) {
  
  # this function only returns a file object when user refuses to overwrite it
  file <- api_trash_filename(filename)
  if (is.file(file)) return(file)
  
  x <- plotly_build(x)[["x"]]
  
  # in v2, traces must reference grid data, so create grid references first
  # http://moderndata.plot.ly/simple-rest-apis-for-charts-and-datasets/
  for (i in seq_along(x$data)) {
    x$data[[i]] <- api_srcify(x$data[[i]])
  }
  
  # same for animation frames
  for (i in seq_along(x$frames)) {
    frame <- x$frames[[i]]
    for (j in seq_along(frame$data)) {
      x$frames[[i]]$data[[j]] <- api_srcify(frame$data[[j]])
    }
  }
  
  sharing <- match.arg(sharing)
  
  bod <- compact(list(
    figure = compact(x[c("data", "layout", "frames")]),
    filename = filename,
    world_readable = identical(sharing, "public"),
    share_key_enabled = identical(sharing, "secret"),
    ...
  ))
  
  res <- api("plots", "POST", to_JSON(bod))
  prefix_class(res$file, c("api_plot", "api_file"))
}

api_create_grid <- function(x, filename = NULL,
                            sharing = c("public", "private", "secret"), ...) {
  
  # this function only returns a file object when user refuses to overwrite it
  file <- api_trash_filename(filename)
  if (is.file(file)) return(file)
  
  sharing <- match.arg(sharing)
  
  bod <- compact(list(
    data = df2grid(x),
    filename = filename,
    world_readable = identical(sharing, "public"),
    share_key_enabled = identical(sharing, "secret"),
    ...
  ))
  
  res <- api("grids", "POST", to_JSON(bod))
  prefix_class(res$file, c("api_grid", "api_file"))
}


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

api_trash_filename <- function(filename = NULL) {
  file <- api_lookup_file(filename)
  if (is.null(file)) return(NULL)
  overwrite <- readline(
    sprintf("I found a file already named '%s' on your account. Overwrite it? [y/n]", filename)
  )
  # default to overwrite, so that you can overwrite non-interactively
  if (identical(overwrite, "n")) {
    message("Returning the file '", filename, "' without overwriting it.")
    return(prefix_class(file, "api_file"))
  }
  api_trash_file(file)
}

api_lookup_file <- function(filename = NULL) {
  file <- tryNULL(api(paste0("files/lookup?path=", filename)))
  if (is.null(file)) file else prefix_class(file, "api_file")
}

api_trash_file <- function(file) {
  if (!is.file(file)) {
    stop("Can't trash a non-file object:", call. = FALSE)
  }
  # make *really* sure before trashing a folder
  if (identical("fold", file$filetype)) {
    prompt <- readline(
      sprintf(
        "'%s' is a folder. Trash it *and* all of its children? [y/n]", 
        file$filename
      )
    )
    if (!identical(prompt, "y")) {
      message("Ok, I won't trash this folder.")
      return(file)
    }
  }
  # TODO: remind user they can recover files?
  endpoint <- sprintf("files/%s/trash", file$fid)
  res <- api(endpoint, "POST")
  invisible(TRUE)
}

# upload a grid of data array attributes, attach src references to the trace,
# and remove the actual data from trace
api_srcify <- function(trace) {
  Attrs <- Schema$traces[[trace[["type"]]]]$attributes
  isArray <- vapply(
    Attrs, function(x) tryFALSE(identical(x[["valType"]], "data_array")), logical(1)
  )
  arrayOK <- vapply(Attrs, function(x) tryFALSE(isTRUE(x[["arrayOk"]])), logical(1))
  grid <- trace[names(trace) %in% names(Attrs)[isArray | arrayOK]]
  # create the grid and replace actual data with "src pointers"
  if (length(grid)) {
    resp <- api_create_grid(grid, filename = new_id())
    fid <- resp[["fid"]]
    cols <- resp[["cols"]]
    for (j in seq_along(cols)) {
      col <- cols[[j]]
      trace[[paste0(col$name, "src")]] <- paste(fid, col$uid, sep = ":")
      trace[[col$name]] <- NULL
    }
  }
  trace
}

# transform a data frame to plotly's grid schema
df2grid <- function(df) {
  idx <- seq_len(ncol(df) %||% length(df)) - 1
  columns <- Map(function(x, y) list(data = x, order = y), df, idx)
  list(cols = columns)
}

api_expect_filetype <- function(file, type = "plot") {
  if (!is.file(file)) {
    stop("Can't read the filetype of a non-file object:", call. = FALSE)
  }
  if (!identical(type, file[["filetype"]])) {
    stop(
      sprintf("This file is of filetype '%s', not '%s'", file[["filetype"]], type),
      call. = FALSE
    )
  }
  invisible(file)
}

# verify `endpoint` is a valid endpoint
api_check_endpoint <- function(endpoint = "/") {
  # check the endpoint -- is this a fool-proof way to get the "root path"?
  rootpoint <- strsplit(endpoint, "(/|\\?)")[[1]][[1]]
  if (!rootpoint %in% api_endpoints()) {
    stop(
      "`endpoint` must point to one of these endpoints:\n", 
      "'", paste(api_endpoints(), collapse = "', '"), "'",
      call. = FALSE
    )
  }
  endpoint
}


api_endpoints <- function() {
  c(
    "", "search", "files", "grids", "plots", "extras", "folders", "images", 
    "comments", "plot-schema", "users", "memberships", "organizations", 
    "subscriptions", "jupyter-notebooks", "shapefiles", "external-images", 
    "spectacle-presentations", "dashboards", "analysis", "dash-apps"
  )
}

is.file <- function(x) {
  inherits(x, "api_file")
}
is.plot <- function(x) {
  inherits(x, "api_plot")
}
is.grid <- function(x) {
  inherits(x, "api_grid")
}

tryFALSE <- function(expr) {
  tryCatch(expr, error = function(e) FALSE)
}
