#' Tools for working with plotly's REST API (v2)
#' 
#' Convenience functions for working with version 2 of plotly's REST API. The 
#' "lowest-level" function, \code{api()} may be used to 
#' 
#' @param id a filename id. 
#' @param username a plotly username.
#' 
#' @param x An R object to hosted on plotly's web platform. 
#' Can be a plotly/ggplot2 object, a data frame, a list of options, or \code{NULL}.
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
#' 
#' @param endpoint the endpoint (i.e., location) for the request. 
#' To see a list of all available endpoints, call \code{api()}.
#' Any query parameters should be included here (see examples).
#' @param verb name of the HTTP verb to use (as in, \code{\link[httr]{VERB}()}).
#' @param body body of the HTTP request(as in, \code{\link[httr]{VERB}()}).
#' If this value is not already converted to JSON 
#' (via \code{\link[jsonlite]{toJSON}()}), it uses the internal \code{to_JSON()}
#' to ensure values are "automatically unboxed" (i.e., vec.
#'
#' @param ... arguments passed onto relevant methods. For \code{api()}, these 
#' arguments are passed onto \code{\link[httr]{VERB}()}.
#' 
#' @export
#' @rdname api
#' @author Carson Sievert
#' @references \url{https://api.plot.ly/v2}
#' @seealso \code{\link{signup}()}
#' @examples 
#' 
#' \dontrun{
#' 
#' # ------------------------------------------------------------
#' # api_create() makes it easy to upload ggplot2/plotly objects
#' # and/or data frames to your plotly account
#' # ------------------------------------------------------------
#' 
#' # A data frame creates a plotly "grid". Printing one will take you 
#' # to the it's web address so you can start creating!
#' (m <- api_create(mtcars))
#' 
#' # A plotly/ggplot2 object create a plotly "plot".
#' p <- plot_ly(mtcars, x = ~factor(vs))
#' (r <- api_create(p))
#' 
#' # api_create() returns metadata about the remote "file". Here is
#' # one way you could use that metadata to download a plot for local use:
#' fileID <- strsplit(r$file$fid, ":")[[1]]
#' layout(
#'   api_download_plot(fileID[2], fileID[1]),
#'   title = sprintf("Local version of <a href='%s'>this</a> plot", r$file$web_url)
#' )
#'
#' ------------------------------------------------------------
#' # The api() function provides a low-level interface for performing 
#' # any action at any endpoint! It always returns a list.
#' # ------------------------------------------------------------
#' 
#' # list all the endpoints
#' api()
#' 
#' # Any query parameters must be included in the endpoint 
#' # see https://api.plot.ly/v2/search
#' api("search?q=overdose")
#' 
#' api("search?q=plottype:pie trump fake")
#' 
#' # some user specific info
#' usr <- Sys.getenv("plotly_username", NA)
#' if (!is.na(usr)) {
#'   # account info https://api.plot.ly/v2/#users
#'   api(sprintf("users/%s", usr))
#'   # list your folders/files https://api.plot.ly/v2/folders#user
#'   api(sprintf("folders/home?user=%s", usr))
#' }
#' 
#' # Retrieve a specific file https://api.plot.ly/v2/files#retrieve
#' api("files/cpsievert:14681")
#' # Copy a file https://api.plot.ly/v2/files#lookup
#' api("files/cpsievert:14681/copy", "POST")
#' # Create a folder https://api.plot.ly/v2/folders#create
#' api("folders", "POST", list(path = "/starts/at/root/and/ends/here"))
#' 
#' }
#' 


#' @rdname api
#' @export
api_create <- function(x = last_plot(), filename = NULL,
                       sharing = c("public", "private", "secret"), ...) {
  UseMethod("api_create")
}


#' @rdname api
#' @export
api_create.plotly <- api_create_plot

#' @rdname api
#' @export
api_create.ggplot <- api_create_plot

#' @rdname api
#' @export
api_create.data.frame <- api_create_grid




#' @rdname api
#' @export
api_download_plot <- function(id, username) {
  f <- api_download_file(id, username)
  api_expect_filetype(f, "plot")
  
  as_widget(
    api_download_file(id, username, "plots", "content?inline_data=true")
  )
}


#' @rdname api
#' @export
api_download_grid <- function(id, username) {
  f <- api_download_file(id, username)
  api_expect_filetype(f, "grid")
  
  prefix_class(
    api_download_file(id, username, "grids"), "api_grid_local"
  )
}

# TODO: should this be exposed to users?
api_download_file <- function(id, username, endpoint = "files", ...) {
  if (missing(id)) stop("Please provide a figure id number")
  if (missing(username)) username <- verify("username")
  fid <- paste(username, id, sep = ":")
  api(file.path(endpoint, fid, ...))
}




#' @rdname api
#' @export
api <- function(endpoint = "/", verb = "GET", body = NULL, ...) {
  api_check_endpoint(endpoint)
  api_check_verb(verb)
  
  # construct the url
  url <- httr::modify_url(
    get_domain("api"), 
    scheme = "https",
    # TODO: should anything else in the endpoint (besides whitespace) be escaped?
    path = file.path("v2", gsub("\\s+", "+", endpoint))
  )
  
  # default to unboxing (i.e., no arrays of length 1)
  if (!is.null(body) && !inherits(body, "json")) {
    body <- to_JSON(body)
  }
  
  resp <- httr::VERB(
    verb = verb, url = url, api_headers(), api_auth(), 
    body = body, ...
  )
  
  structure(process(resp), class = "api")
}
