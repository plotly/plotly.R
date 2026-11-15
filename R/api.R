api_create_plot <- function(x = last_plot(), filename = NULL, fileopt = "overwrite",
                            sharing = "public", ...) {
  
  x <- plotly_build(x)[["x"]]
  
  # filename can be of length 2 (in that case, first filename is plotname)
  len <- length(filename)
  plotname <- if (len > 1) filename[[1]] else filename
  gridname <- if (len > 1) filename[[2]] else if (len == 1) paste(filename, "Grid")
  
  # if file already exists, determine if we can overwrite it
  origfile <- api_lookup_file(plotname)
  overwrite <- is.file(origfile) && identical(fileopt, "overwrite")
  if (overwrite && !identical(origfile$filetype, "plot")) {
    stop(
      sprintf("Can overwrite a file of type '%s' with a plot", origfile$filetype),
      call. = FALSE
    )
  }
  
  # retrieve the parent path, and ensure it exists
  parent_path <- api_pave_path(plotname)
  
  # in v2, traces must reference grid data, so create grid references first
  # http://moderndata.plot.ly/simple-rest-apis-for-charts-and-datasets/
  x <- api_srcify(x, filename = gridname, fileopt = fileopt, sharing = sharing)
  
  bod <- compact(list(
    figure = compact(x[c("data", "layout", "frames")]),
    filename = if (!is.null(plotname)) basename(plotname),
    parent_path = if (!is.null(parent_path)) parent_path,
    world_readable = identical(sharing, "public"),
    share_key_enabled = identical(sharing, "secret"),
    ...
  ))
  
  # overwrite the original file; otherwise, let plotly create it
  res <- if (overwrite) {
    
    message(sprintf(
      "Found a plot already named: '%s'. Since fileopt='overwrite', I'll try to update it", 
      origfile$filename
    ))
    api(paste0("plots/", origfile$fid), "PATCH", to_JSON(bod))
    
  } else {
    
    api("plots", "POST", to_JSON(bod))$file
    
  }
  
  prefix_class(res, c("api_plot", "api_file"))
}

api_create_grid <- function(x, filename = NULL, fileopt = "overwrite",
                            sharing = "public", ...) {
  
  # if file already exists, determine if we can overwrite it
  origfile <- api_lookup_file(filename)
  overwrite <- is.file(origfile) && identical(fileopt, "overwrite")
  if (overwrite && !identical(origfile$filetype, "grid")) {
    stop(
      sprintf("Can overwrite a file of type '%s' with a grid", origfile$filetype),
      call. = FALSE
    )
  }
  
  # retrieve the parent path, and ensure it exists
  parent_path <- api_pave_path(filename)
  
  bod <- compact(list(
    data = df2grid(x),
    filename = if (!is.null(filename)) basename(filename),
    parent_path = if (!is.null(parent_path)) parent_path,
    world_readable = identical(sharing, "public"),
    share_key_enabled = identical(sharing, "secret"),
    ...
  ))
  
  # At least for now, 'overwrite' really means append new columns
  # It shouldn't be so convoluted/hard to update a grid! -- https://api.plot.ly/v2/grids#col
  res <- if (overwrite) {
    
    message(sprintf(
      "Found a grid already named: '%s'. Since fileopt='overwrite', I'll try to update it", 
      origfile$filename
    ))
    cols <- bod$data$cols
    colz <- Map(function(x, y) {
      list(name = paste0(x, "-", new_id()), data = y$data)
    }, names(cols), cols)
    colString <- as.character(to_JSON(setNames(colz, NULL)))
    resp <- api(sprintf("grids/%s/col", origfile$fid), "POST", to_JSON(list(cols = colString)))
    modify_list(origfile, resp)
    
  } else {
    
    api("grids", "POST", to_JSON(bod))$file
    
  }
  
  prefix_class(res, c("api_grid", "api_file"))
}


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

api_lookup_file <- function(filename = NULL) {
  file <- tryNULL(api(paste0("files/lookup?path=", filename)))
  if (is.null(file)) file else prefix_class(file, "api_file")
}

# returns the "parent" directory of the filename (and NULL if none exists)
api_pave_path <- function(filename = NULL) {
  parent <- dirname(filename %||% ".")
  # no directory required
  if (identical(parent, ".")) return(NULL)
  # does this directory already exist?
  file <- api_lookup_file(parent)
  if (is.file(file)) return(parent)
  fold <- api("folders", "POST", list(path = parent))
  parent
}

#api_trash_file <- function(file) {
#  if (!is.file(file)) stop("Can't trash a non-file object:", call. = FALSE)
#  res <- api(sprintf("files/%s/trash", file$fid), "POST")
#  res2 <- api(sprintf("files/%s/permanent_delete", file$fid), "DELETE")
#  invisible(TRUE)
#}

# upload *one* grid of data array attributes, and replace actual trace data
# with src/uid references 
api_srcify <- function(p, filename = new_id(), fileopt = "overwrite", sharing = "public") {
  
  # track the trace/frame index with official element names
  # this makes it easier to map uids to attributes (& vice versa)
  p$data <- setNames(p$data, seq_along(p$data))
  for (i in seq_along(p$frames)) {
    p$frames <- setNames(p$frames, seq_along(p$frames))
    d <- p$frames[[i]]$data
    p$frames[[i]]$data <- setNames(d, seq_along(d))
  }
  
  # grab just the src-able attributes
  pSrc <- filter_to_src(p)
  
  # flatten the list
  # http://stackoverflow.com/questions/42739419/r-convert-nested-list-into-a-one-level-list
  pSrcFlat <- lapply(rapply(pSrc, enquote, how = "unlist"), function(x) I(eval(x)))
  
  if (length(pSrcFlat)) {
    
    resp <- api_create_grid(
      pSrcFlat, filename = filename, 
      fileopt = fileopt, sharing = sharing
    )
    
    # replace data values with their uid references 
    idx <- strsplit(names(pSrcFlat), "\\.")
    idxSrc <- strsplit(paste0(names(pSrcFlat), "src"), "\\.")
    for (i in seq_along(pSrcFlat)) {
      p <- re_place(p, idx[[i]], NULL)
      uid <- paste(resp$fid, resp[["cols"]][[i]]$uid, sep = ":")
      p <- re_place(p, idxSrc[[i]], uid)
    }
  }
  
  # restore sanity
  p$data <- setNames(p$data, NULL)
  for (i in seq_along(p$frames)) {
    p$frames <- setNames(p$frames, NULL)
    p$frames[[i]]$data <- setNames(p$frames[[i]]$data, NULL)
  }
  
  p
}

filter_to_src <- function(x) {
  if (!is.list(x)) {
    val <- if (isTRUE(attr(x, "apiSrc"))) x else NULL
    return(val)
  }
  rmNullObs(lapply(x, filter_to_src))
}

# http://stackoverflow.com/questions/26539441/r-remove-null-elements-from-list-of-lists
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
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
