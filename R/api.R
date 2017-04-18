api_create_plot <- function(x = last_plot(), filename = NULL,
                            sharing = c("public", "private", "secret"), ...) {
  
  fileINFO <- tryNULL(api(paste0("files/lookup?path=", filename)))
  if (!is.null(fileINFO)) {
    warning(
      "A file with this name already exists. Returning that file... \n", 
      "If you want to create a new plot, specify a new filename. \n",
      "If you want to overwrite this plot, use plot_overwrite().", call. = FALSE
    )
    return(fileINFO)
  }
  
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
  
  sharing <- match.arg(sharing, sharing)
  isPublic <- identical(sharing, "public")
  
  bod <- to_JSON(
    list(
      figure = x[c("data", "layout", "frames")],
      world_readable = isPublic,
      share_key_enabled = identical(sharing, "secret"),
      ...
    )
  )
  
  # TODO: do we need to PATCH to ensure this is secret?
  prefix_class(
    api("plots", "POST", bod), "api_plot"
  )
}


api_create_grid <- function(x, filename = NULL,
                            sharing = c("public", "private", "secret"), ...) {
  
  fileINFO <- tryNULL(api(paste0("files/lookup?path=", filename)))
  if (!is.null(fileINFO)) {
    warning(
      "A file with this name already exists. Returning that file... \n", 
      "  If you want to create a new grid, specify a new filename. \n",
      "  If you want to overwrite a grid, use grid_overwrite().", call. = FALSE
    )
    return(fileINFO)
  }
  
  sharing <- match.arg(sharing, sharing)
  isPublic <- identical(sharing, "public")
  
  bod <- to_JSON(list(
    data = df2grid(x),
    world_readable = isPublic,
    share_key_enabled = identical(sharing, "secret"),
    ...
  ))
  
  prefix_class(
    api("grids", "POST", bod), "api_grid"
  )
}







# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------


# upload a grid of data array attributes, attach src references to the trace,
# and remove the actual data from trace
api_srcify <- function(trace) {
  Attrs <- Schema$traces[[trace[["type"]]]]$attributes
  isArray <- sapply(Attrs, function(x) {
    tryCatch(identical(x[["valType"]], "data_array"), error = function(e) FALSE)
  })
  grid <- trace[names(trace) %in% names(Attrs)[isArray]]
  # create the grid and replace actual data with "src pointers"
  if (length(grid)) {
    resp <- api_create_grid(grid, filename = new_id())
    fid <- resp[["file"]][["fid"]]
    cols <- resp[["file"]][["cols"]]
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


api_expect_filetype <- function(f = NULL, type = "plot") {
  if (!identical(type, f[["filetype"]])) {
    stop(
      sprintf("This file is of filetype '%s', not '%s'", f[["filetype"]] %||% "", type),
      call. = FALSE
    )
  }
  invisible(f)
}

# verify `method` is an httr verb
api_check_verb <- function(verb) {
  is_httr_verb <- any(vapply(api_verbs(), identical, logical(1), verb))
  if (!is_httr_verb) {
    warning("Didn't recognize verb:", verb, call. = FALSE)
  }
  verb
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

api_verbs <- function() {
  c("DELETE", "BROWSE", "GET", "HEAD", "PATCH", "POST", "PUT", "VERB")
}

api_endpoints <- function() {
  c(
    "", "search", "files", "grids", "plots", "extras", "folders", "images", 
    "comments", "plot-schema", "users", "memberships", "organizations", 
    "subscriptions", "jupyter-notebooks", "shapefiles", "external-images", 
    "spectacle-presentations", "dashboards", "analysis", "dash-apps"
  )
}

