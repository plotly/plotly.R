#' Create/Modify plotly graphs
#'
#' Deprecated: see \link{plots_create}.
#'
#' @param x either a ggplot object, a plotly object, or a list.
#' @param filename character string describing the name of the plot in your 
#' plotly account. Use / to specify directories. If a directory path does not 
#' exist it will be created. If this argument is not specified and the title 
#' of the plot exists, that will be used for the filename.
#' @param fileopt character string describing whether to create a "new" plotly, 
#' "overwrite" an existing plotly, "append" data to existing plotly, 
#' or "extend" it.
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
#' @export
#' @seealso \code{\link{plot_ly}()}, \code{\link{signup}()}
plotly_POST <- function(x = last_plot(), filename = NULL, fileopt = "overwrite", 
                        sharing = c("public", "private", "secret"), ...) {
  .Deprecated("plots_create")
  plots_create(x, filename, fileopt, sharing, ...)
}

#' Request a figure object
#' 
#' Deprecated: see \link{plots_get}.
#' 
#' @param username corresponding username for the figure.
#' @param id of the Plotly figure.
#' @export
get_figure <- function(username, id) {
  .Deprecated("plots_get")
  plots_get(id, username)
}

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
