#' Main interface to plotly 
#' 
#' Deprecated: see \link{signup} for credentials/configuration storage details.
#' See \link{ggplotly} for the new ggplot2 interface.
#' 
#' @param username plotly username
#' @param key plotly API key
#' @export
plotly <- function(username, key) {
  
  if (!missing(username)) {
    message("Storing 'username' as the environment variable 'plotly_username'")
    Sys.setenv("plotly_username" = username)
  } else {
    usr <- verify("username")
  }
  if (!missing(key)) {
    message("Storing 'key' as the environment variable 'plotly_api_key'")
    Sys.setenv("plotly_api_key" = key)
  } else {
    key <- verify("api_key")
  }
  
  .Deprecated("ggplotly")
  .Deprecated("plot_ly")
  invisible(NULL)
}


#' Plotly Offline
#' 
#' Deprecated in version 2.0 (offline plots are now the default)
#' 
#' @param p a plotly object
#' @param height A valid CSS unit. (like "100\%", "600px", "auto") or a number, 
#' which will be coerced to a string and have "px" appended.
#' @param width A valid CSS unit. (like "100\%", "600px", "auto") or a number, 
#' which will be coerced to a string and have "px" appended.
#' @param out_dir a directory to place the visualization. 
#' If \code{NULL}, a temporary directory is used when the offline object is printed.
#' @param open_browser open the visualization after creating it?
#' @author Carson Sievert
#' @return a plotly object of class "offline"
#' @export

offline <- function(p, height, width, out_dir, open_browser) {
  .Deprecated("plot_ly")
  p
}
