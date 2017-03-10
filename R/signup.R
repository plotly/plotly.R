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
