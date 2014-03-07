#' Sign up to plotly.
#' 
#' A sign up interface to Plotly through the R Console. See documentation and 
#' examples at https://plot.ly/API
#' 
#' @import RCurl RJSONIO
#' @param username Desired username
#' @param email Desired email
#' @details See documentation and examples at https://plot.ly/API
#' @return
#' \itemize{
#'  \item api_key key to use with the api
#'  \item tmp_pw temporary password to access your plotly account
#' }
#' @references https://plot.ly/API
#' @author Chris Parmer chris@@plot.ly
#' @note https://plot.ly/API
#' @export
#' @examples \dontrun{
#' username <- 'anna.lyst'
#' email <- 'anna.lyst@@plot.ly'
#' response <- signup(username, email)
#' response$api_key # key to access plotly with
#' response$tmp_pw # temporary password to access your plotly account
#' }

signup <- function(username=NULL, email=NULL){
  if(is.null(username))
    key <- getOption("plotlyUsername", stop("you need a user name for Plot.ly - See the signup function"))
  if(is.null(key))
    key <- getOption("plotlyKey", stop("you need an API key for Plot.ly - See the signup function"))
  
  platform = 'R'
  version = as.character(packageVersion("plotly"))
  url = 'https://plot.ly/apimkacct'
  options(RCurlOptions = list(sslversion=3, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  respst = postForm(url,
            platform=platform,
            version=version,
            email=email,
            un=username)
  resp <- fromJSON(respst, simplify=FALSE)
  if(!is.null(resp$filename)) pub$filename = resp$filename
  if(!is.null(resp$error)) cat(resp$err)
  if(!is.null(resp$warning)) cat(resp$warning)
  if(!is.null(resp$message)) cat(resp$message)
  return(resp)
}