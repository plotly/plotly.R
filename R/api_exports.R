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
#' @param x either a plotly object, a ggplot object, a data frame, or a list.
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
#' (m <- upload(mtcars))
#' 
#' # or download data from the platform into R
#' download("14705", "cpsievert")
#' 
#' 
#' # upload a plot to plotly's web platforam
#' p <- plot_ly(mtcars, x = ~factor(vs))
#' r <- upload(p, filename = "mtcars-bar-plot")
#' # note that you can always obtain information about the remote figure
#' str(r)
#' 
#'  # TODO: overwrite() examples
#' 
#' # or pull a plot down from the platform and work with it locally
#' download("14708", "cpsievert") %>%
#'   layout(title = "A Custom Title")
#' 
#' # outside of data frames and plotly/ggplot objects,
#' # you 
#' 
#' dash <- jsonlite::fromJSON("https://plot.ly/~jackluo/430.json")
#' upload(
#'   dash, endpoint = "users", "cats"
#' )
#' 
#' upload(
#'   x = NULL, endpoint = "users", "cats"
#' )
#' 
#' 
#' # TODO: overwrite() examples
#' 
#' 
#' 
#' 
#' 
#' }
#' 


#' @export
#' @rdname api
upload <- function(x = NULL, ...) {
  UseMethod("upload")
}

#' @export
#' @rdname api
upload.plotly <- upload_plot

#' @export
#' @rdname api
upload.ggplot <- upload_plot

#' @export
#' @rdname api
upload.data.frame <- upload_grid

#' @export
#' @rdname api
upload.list <- function(x = NULL, endpoint, ...) {
  upload.json(to_JSON(x), endpoint = endpoint, ...)
}

#' @export
#' @rdname api
upload.json <- function(x = NULL, endpoint, ...) {
  
  # TODO: check endpoint arg value?
  resp <- httr::POST(
    url = file.path(get_domain("api"), "v2", endpoint, ...),
    body = x, api_headers(), api_auth()
  )
  
  structure(process(resp), class = "api")
}


#' @export
#' @rdname api
download <- function(id, username) {
  UseMethod("download")
}

#' @export
#' @rdname api
download.default <- function(id, username) {
  
  # figure out the type of file first (i.e., grid or plot)
  f <- download_file(id, username)
  type <- f$filetype %||% "plot"
  if (!type %in% c("grid", "plot")) {
    warning(
      "Sorry, we currently support downloads of just plots & grids, not ", type,
      "\n Please file an issue if you'd like support for this filetype:\n",
      "https://github.com/ropensci/plotly/issues/new",
      call. = FALSE
    )
  }
  
  download_func <- if (type == "plot") {
    download_plot
  } else if (type == "grid") {
    download_grid
  } else {
    download_file
  }
  
  download_func(id, username)
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
