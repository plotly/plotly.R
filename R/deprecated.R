#' Main interface to plotly 
#' 
#' @description This function is now deprecated. It used to provide a way to store plotly 
#' account credentials, but that can now be done with environment variables. 
#' For more details and examples, see \url{https://plot.ly/r/getting-started/}.
#' 
#' If you're here looking for an intro/overview of the package, see the 
#' \href{https://github.com/ropensci/plotly/#getting-started}{readme}
#' 
#' @keywords internal
#' @param username plotly username
#' @param key plotly API key
#' @export
#' @seealso \code{\link{ggplotly}()}, \code{\link{plot_ly}()}, \code{\link{signup}()}
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


#' Convert a plotly object to an htmlwidget object
#' 
#' This function was deprecated in  4.0.0, 
#' as plotly objects are now htmlwidget objects,
#' so there is no need to convert them.
#' 
#' @param x a plotly object.
#' @param ... other options passed onto \code{htmlwidgets::createWidget}
#' @export

as.widget <- function(x, ...) {
  .Deprecated("plot_ly")
  x
}

# for legacy reasons
toWidget <- as.widget
