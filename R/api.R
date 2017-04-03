# ---------------------------------------------------------------------------
# Uploading
# ---------------------------------------------------------------------------

upload_plot <- function(x = last_plot(), filename = NULL,
                        sharing = c("public", "private", "secret"), ...) {
  
  if (!is.null(file <- file_lookup(filename))) {
    warning(
      "A file with this name already exists. Returning that file... \n", 
      "If you want to create a new plot, specify a new filename. \n",
      "If you want to overwrite this plot, use plot_overwrite().", call. = FALSE
    )
    return(prefix_class(file, "api_plot"))
  }
  
  x <- plotly_build(p)[["x"]]
  
  # in v2, traces must reference grid data, so create grid references first
  # http://moderndata.plot.ly/simple-rest-apis-for-charts-and-datasets/
  for (i in seq_along(x$data)) {
    x$data[[i]] <- srcify(x$data[[i]])
  }
  
  # same for animation frames
  for (i in seq_along(x$frames)) {
    frame <- x$frames[[i]]
    for (j in seq_along(frame$data)) {
      x$frames[[i]]$data[[j]] <- srcify(frame$data[[j]])
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
  
  resp <- httr::POST(
    url = file.path(get_domain("api"), "v2", "plots"),
    body = bod, api_headers(), api_auth()
  )
  
  prefix_class(httr::content(resp), "api_plot")
}


upload_grid <- function(x, filename = as.character(substitute(x)),
                        sharing = c("public", "private", "secret"), ...) {
  
  # Does this file already exist?
  if (!is.null(file <- file_lookup(filename))) {
    warning(
      "A file with this name already exists. Returning that file... \n", 
      "  If you want to create a new grid, specify a new filename. \n",
      "  If you want to overwrite a grid, use grid_overwrite().", call. = FALSE
    )
    return(prefix_class(file, "api_grid"))
  }
  
  sharing <- match.arg(sharing, sharing)
  isPublic <- identical(sharing, "public")
  
  bod <- to_JSON(list(
    data = df2grid(x),
    world_readable = isPublic,
    share_key_enabled = identical(sharing, "secret"),
    ...
  ))
  
  resp <- httr::POST(
    file.path(get_domain("api"), "v2", "grids"),
    body = bod, api_headers(), api_auth()
  )
  
  prefix_class(process(resp), "api_grid")
}


# ---------------------------------------------------------------------------
# Downloading
# ---------------------------------------------------------------------------

download_plot <- function(id, username) {
  as_widget(
    download_file(id, username, "plots", "content?inline_data=true")
  )
}

download_grid <- function(id, username) {
  prefix_class(
    download_file(id, username, "grids"), 
    "api_grid_local"
  )
}

# TODO: should files_get() be exposed to users?
download_file <- function(id, username, endpoint = "files", ...) {
  if (missing(id)) stop("Please provide a figure id number")
  if (missing(username)) username <- verify("username")
  fid <- paste(username, id, sep = ":")
  url <- file.path(get_domain("api"), "v2", endpoint, fid, ...)
  resp <- httr::GET(url, api_headers(), api_auth())
  process(resp)
}

# ---------------------------------------------------------------------------
# Overwriting
# ---------------------------------------------------------------------------

overwrite_plot <- function() {
  
}

overwrite_grid <- function(x, filename = as.character(substitute(x)), ...) {
  
}

# ---------------------------------------------------------------------------
# Listing?
# ---------------------------------------------------------------------------

list_files <- function(username) {
  if (missing(username)) username <- verify("username")
  url <- file.path(get_domain("api"), "v2", "folders", paste0("home?user=", username))
  resp <- httr::GET(url, api_headers(), api_auth())
  process(resp)
}


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

file_lookup <- function(path) {
  url <- file.path(get_domain("api"), "v2", "files", paste0("lookup?path=", path))
  resp <- httr::GET(url, api_headers(), api_auth())
  if (httr::http_error(resp)) {
    return(NULL)
  }
  httr::content(resp)
}

# upload a grid of data array attributes, attach src references to the trace,
# and remove the actual data from trace
srcify <- function(trace) {
  Attrs <- Schema$traces[[trace[["type"]]]]$attributes
  isArray <- sapply(Attrs, function(x) {
    tryCatch(identical(x[["valType"]], "data_array"), error = function(e) FALSE)
  })
  grid <- trace[names(trace) %in% names(Attrs)[isArray]]
  # create the grid and replace actual data with "src pointers"
  if (length(grid)) {
    resp <- grid_upload(grid, filename = new_id())
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
