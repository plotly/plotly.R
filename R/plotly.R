#' Initiate a plotly object
#'
#' Creates a plotly object with a single trace and an empty layout. To add traces, 
#' see \code{\link{add_trace}()} and to customize the layout see \code{\link{layout}()}.
#' 
#' @param data A data frame to visualize (optional).
#' @param type A charater string describing the type of trace.
#' @param ... Trace properties. See the references section below for documentation
#' of these properties.
#' @param inherit should future traces inherit properties from this initial trace?
#' @author Carson Sievert
#' @references 
#'   \url{https://plot.ly/javascript-graphing-library/reference/#Trace_objects}
#' @export
#' @examples
#' 
#' data(economics, package = "ggplot2")
#' # basic time-series plot
#' p <- plot_ly(economics, x = date, y = uempmed, type = "scatter", showlegend = F)
#' # add a loess smoother
#' p2 <- add_trace(p, y = fitted(loess(uempmed ~ as.numeric(date))))
#' # add a title
#' p3 <- layout(p2, title = "Median duration of unemployment (in weeks)")
#' # change the font
#' layout(p3, font = list(family = "Courier New, monospace"))
#' 
#' # sometimes, a data frame isn't fit for the use case...
#' # for 3D surface plots, a numeric matrix is more natural
#' plot_ly(z = volcano, type = "surface")
#' 
plot_ly <- function(data = data.frame(), type = "scatter", ..., inherit = TRUE) {
  # record trace information
  tr <- list(
    type = type,
    # TODO: verify/filter arguments based on trace type.
    args = substitute(list(...)),
    env = list2env(data),
    enclos = parent.frame(),
    inherit = inherit
  )
  # this info is sufficient for recreating the plot
  p <- list(
    data = list(tr),
    # Maybe provide an argument to keep layout?
    layout = NULL,
    url = NULL
  )
  hash_plot(data, p)
}

#' Add a trace to a plotly object
#'
#' Turns a dataset into a plotly object. A plotly object can be conceptualized as
#' a set of traces, a layout. This function will initiate 
#' 
#' @param data A data frame with a class of plotly.
#' @param ... Trace arguments. Arguments are evaluated in the environment attached to 
#' the most recent trace. See the reference below for documentation.
#' @author Carson Sievert
#' @export
#' @references 
#'   \url{https://plot.ly/javascript-graphing-library/reference/#Trace_objects}
#' 
add_trace <- function(data = data.frame, ...) {
  tr <- list(
    args = substitute(list(...)),
    env = list2env(data),
    enclos = parent.frame()
  )
  p <- get_plot(data)
  p$data <- c(p$data, list(tr))
  hash_plot(data, p)
}

# Layout and layout style objects
# https://plot.ly/javascript-graphing-library/reference/#Layout_and_layout_style_objects

#' Add and/or modify layout of a plotly
#' 
#' @inheritParams add_trace
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#layout}
#' 
layout <- function(data = data.frame(), ...) {
  layout <- list(
    args = substitute(list(...)),
    env = list2env(data),
    enclos = parent.frame()
  )
  p <- get_plot(data)
  p$layout <- c(p$layout, list(layout))
  hash_plot(data, p)
}

#' Modify trace styling
#'
#' @param data A data frame with a class of plotly.
#' @param traces numeric vector. Which traces should be modified?
#' @param ... arguments coerced to a list and used to modify trace(s)
#' @author Carson Sievert
#' @export
#'
style <- function(data = data.frame(), traces = 1, ...) {
  style <- list(
    args = substitute(list(...)),
    env = list2env(data),
    enclos = parent.frame(),
    traces = traces
  )
  p <- get_plot(data)
  p$style <- c(p$style, list(style))
  hash_plot(data, p)
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
