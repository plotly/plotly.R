api_create_plot <- function(x = last_plot(), filename = NULL,
                            sharing = c("public", "private", "secret"), ...) {
  
  x <- plotly_build(x)[["x"]]
  
  # filename can be of length 2 (in that case, first filename is plotname)
  len <- length(filename)
  plotname <- if (len > 1) filename[[1]] else filename
  gridname <- if (len > 1) filename[[2]] else if (len == 1) paste(filename, "Grid")
  
  # returns a file object *only* when user refuses to overwrite it
  file <- api_trash_filename(plotname)
  if (is.file(file)) return(file)
  
  # retrieve the parent path, and ensure it exists
  parent_path <- api_pave_path(plotname)
  
  # in v2, traces must reference grid data, so create grid references first
  # http://moderndata.plot.ly/simple-rest-apis-for-charts-and-datasets/
  x <- api_srcify(x, filename = gridname, sharing = sharing)
  
  sharing <- match.arg(sharing)
  
  bod <- compact(list(
    figure = compact(x[c("data", "layout", "frames")]),
    filename = if (!is.null(plotname)) basename(plotname),
    parent_path = if (!is.null(parent_path)) parent_path,
    world_readable = identical(sharing, "public"),
    share_key_enabled = identical(sharing, "secret"),
    ...
  ))
  
  res <- api("plots", "POST", to_JSON(bod))
  prefix_class(res$file, c("api_plot", "api_file"))
}

api_create_grid <- function(x, filename = NULL,
                            sharing = c("public", "private", "secret"), ...) {
  
  if (length(filename) > 1) {
    stop("Length of filename must be 1 or 0 (NULL)", call. = FALSE)
  }
  
  # returns a file object *only* when user refuses to overwrite it
  file <- api_trash_filename(filename)
  if (is.file(file)) return(file)
  
  # retrieve the parent path, and ensure it exists
  parent_path <- api_pave_path(filename)
  
  sharing <- match.arg(sharing)
  
  bod <- compact(list(
    data = df2grid(x),
    filename = if (!is.null(filename)) basename(filename),
    parent_path = if (!is.null(parent_path)) parent_path,
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


# upload *one* grid of data array attributes, and replace actual trace data
# with src/uid references 
api_srcify <- function(p, filename = new_id(), sharing = "public") {
  
  # filter to all the "src-ifiable" trace attributes
  srcs <- lapply(p$data, trace_filter_src)
  
  # do the same for frames (if frames is NULL, this returns `list()`)
  srcFrames <- lapply(p$frames, function(f) {
    lapply(f$data, trace_filter_src)
  })
  
  # treat p$data as the "first frame" (i.e., create a list where first level 
  # is "frames", second is "traces", and third is "src attributes")
  srcAll <- c(list(srcs), srcFrames)
  
  # reduce to a list of src attributes
  srcFlat <- Reduce(c, Reduce(c, srcAll))
  
  # create a grid as long as there is >1 src attribute
  if (length(srcFlat)) {
    # TODO: allow user do specify filename of the grid?
    resp <- api_create_grid(srcFlat, filename = filename, sharing = sharing)
    
    # create an index mapping so we can assign a uid back to a frame/trace
    srcMap <- lapply(srcAll, function(frame) { 
      rep(seq_along(frame), lengths(frame))
    })
    
    # sanity check
    if (length(srcFlat) != sum(lengths(srcMap))) {
      stop(
        "Something went wrong. Please report to https://github.com/ropensci/plotly/issues/new",
        call. = FALSE
      )
    }
    
    # replace data values with uid references 
    # TODO: this has to be hopelessly slow with many frames...
    ctr <- 1
    for (i in seq_along(srcMap)) {
      traceIndicies <- srcMap[[i]]
      for (j in seq_along(traceIndicies)) {
        index <- traceIndicies[[j]]
        col <- resp[["cols"]][[ctr]]
        nm <- names(srcFlat)[[ctr]]
        if (i > 1) {
          p$frames[[i - 1]]$data[[index]][[nm]] <- NULL
          p$frames[[i - 1]]$data[[index]][[paste0(nm, "src")]] <- paste(resp$fid, col$uid, sep = ":")
        } else {
          p$data[[index]][[nm]] <- NULL
          p$data[[index]][[paste0(nm, "src")]] <- paste(resp$fid, col$uid, sep = ":")
        }
        ctr <- ctr + 1
      }
    }
    
  }
  
  p
}

# filter a trace down to it's "src-able" attributes
trace_filter_src <- function(trace) {
  if (!length(trace)) return(trace)
  type <- trace[["type"]] %||% "scatter"
  attrs <- Schema$traces[[type]]$attributes
  srcs <- sub("src$", "", grep("src$", names(attrs), value = TRUE))
  trace[names(trace) %in% srcs]
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
