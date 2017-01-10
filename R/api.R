#' Tools for working with plotly's REST API (v2)
#' 
#' Convenience functions for uploading, downloading, and overwriting 3 
#' kinds of important plotly objects: grids, plots, and dashboards. The actual 
#' REST API exposes 7 kinds of objects, 
#' 
#' plotly's web interface is built entirely upon it's REST API which exposes 3
#' "classes" of important objects: grids, plots, and dashboards.
#' To learn how to use the API more directly, see 
#' \url{http://moderndata.plot.ly/simple-rest-apis-for-charts-and-datasets/}
#' 
#' @param p either a ggplot object, a plotly object, or a list.
#' @param filename character string which names the file.
#' @param sharing If 'public', anyone can view this graph. It will appear in 
#' your profile and can appear in search engines. You do not need to be
#' logged in to Plotly to view this chart.
#' If 'private', only you can view this plot. It will not appear in the
#' Plotly feed, your profile, or search engines. You must be logged in to 
#' Plotly to view this graph. You can privately share this graph with other 
#' Plotly users in your online Plotly account and they will need to be logged 
#' in to view this plot.
#' If 'secret', anyone with this secret link can view this chart. It will
#' not appear in the Plotly feed, your profile, or search engines. 
#' If it is embedded inside a webpage or an IPython notebook, anybody who is 
#' viewing that page will be able to view the graph. 
#' You do not need to be logged in to view this plot.
#' @param id a filename id. 
#' @param username a plotly username.
#' 
#' @rdname api
#' @author Carson Sievert
#' @references \url{https://api.plot.ly/v2}, \url{http://moderndata.plot.ly/simple-rest-apis-for-charts-and-datasets/}
#' @seealso \code{\link{signup}()}
#' @examples
#' \dontrun{
#' # upload a data frame and start creating via plotly's platform
#' m <- upload_grid(mtcars)
#' 
#' # or download data from the platform into R
#' download_grid("14705", "cpsievert")
#' 
#' # upload a plot to plotly's web platforam
#' p <- plot_ly(mtcars, x = ~factor(vs))
#' r <- upload_plot(p, filename = "mtcars-bar-plot")
#' # note that you can always obtain about the remote figure
#' str(r)
#' 
#' # or pull a plot down from the platform and work with it locally
#' download_plot("14708", "cpsievert")
#' 
#' }
#' 


#' @export
#' @rdname api
upload_plot <- function(p = last_plot(), filename = NULL,
                         sharing = c("public", "private", "secret"), ...) {
  
  if (!is.null(file <- file_lookup(filename))) {
    warning(
      "A file with this name already exists. Returning that file... \n", 
      "If you want to create a new plot, specify a new filename. \n",
      "If you want to overwrite this plot, use overwrite_plot().", call. = FALSE
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

#' @export
#' @rdname api
download_plot <- function(id, username) {
  
  if (missing(id)) stop("Please provide a figure id number")
  if (missing(username)) username <- verify("username")
  
  fid <- paste(username, id, sep = ":")
  resp <- httr::GET(
    file.path(get_domain("api"), "v2", "plots", fid, "content?inline_data=true"), 
    api_headers(), api_auth()
  )
  
  as_widget(process(resp))
}

#' @export
#' @rdname api
overwrite_plot <- function() {
  
}


#' @export
#' @rdname api
upload_grid <- function(x, filename = as.character(substitute(x)),
                        sharing = c("public", "private", "secret"), ...) {
  
  # Does this file already exist?
  if (!is.null(file <- file_lookup(filename))) {
    warning(
      "A file with this name already exists. Returning that file... \n", 
      "  If you want to create a new grid, specify a new filename. \n",
      "  If you want to overwrite a grid, use overwrite_grid().", call. = FALSE
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

#' @export
#' @rdname api
overwrite_grid <- function(x, filename = as.character(substitute(x)), ...) {
  
}


#' @export
#' @rdname api
download_grid <- function(id, username) {
  resp <- file_get(id, username, "grids")
  prefix_class(process(resp), "api_grid_local")
}


files_list <- function(username) {
  if (missing(username)) username <- verify("username")
  url <- file.path(get_domain("api"), "v2", "folders", paste0("home?user=", username))
  resp <- httr::GET(url, api_headers(), api_auth())
  
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
    resp <- upload_grid(grid, filename = new_id())
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


# TODO: should files_get() be exposed to users?
file_get <- function(id, username, filetype = "plots") {
  if (missing(id)) stop("Please provide a figure id number")
  if (missing(username)) username <- verify("username")
  fid <- paste(username, id, sep = ":")
  url <- file.path(get_domain("api"), "v2", filetype, fid)
  httr::GET(url, api_headers(), api_auth())
}

# transform a data frame to plotly's grid schema
df2grid <- function(df) {
  idx <- seq_len(ncol(df) %||% length(df)) - 1
  columns <- Map(function(x, y) list(data = x, order = y), df, idx)
  list(cols = columns)
}


#' Create a new plotly account.
#'
#' A sign up interface to plotly through the R Console.
#'
#' @param username Desired username.
#' @param email Desired email.
#' @param save If request is successful, should the username & API key be
#' automatically stored as an environment variable in a .Rprofile?
#'
#' @return
#' \itemize{
#'  \item api_key key to use with the api
#'  \item tmp_pw temporary password to access your plotly account
#' }
#' @references https://plot.ly/rest/
#' @export
#' @examples \dontrun{
#' # You need a plotly username and API key to communicate with the plotly API.
#'
#' # If you don't already have an API key, you can obtain one with a valid
#' # username and email via signup().
#' s <- signup('anna.lyst', 'anna.lyst@@plot.ly')
#'
#' # If you already have a username and API key, please create the following
#' # environment variables:
#' Sys.setenv("plotly_username" = "me")
#' Sys.setenv("plotly_api_key" = "mykey")
#' # You can also change the default domain if you have a plotly server.
#' Sys.setenv("plotly_domain" = "http://mydomain.com")
#'
#' # If you want to automatically load these environment variables when you
#' # start R, you can put them inside your ~/.Rprofile 
#' # (see help(.Rprofile) for more details)
#' 
#' }
signup <- function(username, email, save = TRUE) {
  if (missing(username)) username <- verify("username")
  if (missing(email)) stop("Must specify a valid email")
  # construct body of message to plotly server
  bod <- list(
    un = username,
    email = email,
    platform = "R",
    version = as.character(packageVersion("plotly"))
  )
  base_url <- file.path(get_domain(), "apimkacct")
  resp <- httr::POST(base_url, body = bod)
  con <- process(append_class(resp, "signup"))
  if (save) {
    # store API key as an environment variable in .Rprofile
    cat_profile("username", con$un)
    cat_profile("api_key", con$api_key)
  }
  Sys.setenv("plotly_username" = con$un)
  Sys.setenv("plotly_api_key" = con$api_key)
  invisible(con)
}
