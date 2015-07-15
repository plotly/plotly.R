#' Initiate a plotly visualization
#'
#' Transform data into a plotly visualization.
#' 
#' There are a number of "visual properties" that aren't included in the officical 
#' Reference section (see below). 
#' 
#' @param data A data frame (optional).
#' @param ... Visual properties. 
#' All arguments documented in the references section below are supported.
#' In addition, there are special arguments which map variables to visual
#' aethestics in a similar style to ggplot2 (such as \code{color}).
#' @param type A charater string describing the type of trace.
#' @param group A variable name for mapping to group. 
#' If used, a different trace will be created for each unique value.
#' @param color A variable name for mapping to color.
#' @param colors Either a colorbrewer2.org palette name (e.g. "YlOrRd" or "Blues"), 
#' or a vector of colors to interpolate in hexadecimal "#RRGGBB" format, 
#' or a color interpolation function like \link{grDevices::colorRamp}.
#' @param symbol A variable name for mapping to symbols.
#' @param symbols A character vector of symbol types. Possible values:
#' 'dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up' 
#' 
#' @param inherit should future traces inherit properties from this initial trace?
#' @param evaluate logical. Evaluate arguments when this function is called?
#' @seealso \code{\link{layout}()}, \code{\link{add_trace}()}, \code{\link{style}()}
#' @references \url{https://plot.ly/r/reference/}
#' @author Carson Sievert
#' @export
#' @examples
#' \dontrun{
#' data(economics, package = "ggplot2")
#' # basic time-series plot
#' p <- plot_ly(economics, x = date, y = uempmed, type = "scatter", 
#'   showlegend = FALSE)
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
#' }
#' 
plot_ly <- function(data = data.frame(), ..., type = "scatter",
                    inherit = TRUE, evaluate = FALSE) {
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
  if (evaluate) p <- plotly_build(p)
  hash_plot(data, p)
}

#' Add a trace to a plotly visualization
#' 
#' @param p A plotly visualization.
#' @param ... Visual properties. 
#' All arguments documented in the references section below are supported.
#' In addition, there are special arguments which map variables to visual
#' aethestics in a similar style to ggplot2 (such as \code{color}).
#' @param data A data frame (optional).
#' @param evaluate logical. Evaluate arguments when this function is called?
#' @references \url{https://plot.ly/r/reference/}
#' @author Carson Sievert
#' @export
#' 
add_trace <- function(p = get_plot(), ..., 
                      data = NULL, evaluate = FALSE) {
  p <- get_plot(p)
  tr <- list(
    args = substitute(list(...)),
    # if data is missing, adopt the most recent data environment
    env = if (is.null(data)) p$data[[length(p$data)]]$env else list2env(data),
    enclos = parent.frame()
  )
  p$data <- c(p$data, list(tr))
  if (evaluate) p <- plotly_build(p)
  hash_plot(data, p)
}

#' Add and/or modify layout of a plotly
#' 
#' @inheritParams add_trace
#' @author Carson Sievert
#' @references \url{https://plot.ly/r/reference/#Layout_and_layout_style_objects}
#' @export
#' 
layout <- function(p = get_plot(), ..., 
                   data = NULL, evaluate = FALSE) {
  p <- get_plot(p)
  layout <- list(
    args = substitute(list(...)),
    # if data is missing, adopt the most recent data environment
    env = if (is.null(data)) p$data[[length(p$data)]]$env else list2env(data),
    enclos = parent.frame()
  )
  p$layout <- c(p$layout, list(layout))
  if (evaluate) p <- plotly_build(p)
  hash_plot(data, p)
}

#' Modify trace(s)
#'
#' Modify trace(s) of an existing plotly visualization. Useful when used in
#' conjunction with \code{\link{get_figure}()}.
#'
#' @param p A plotly visualization.
#' @param ... Visual properties.
#' @param traces numeric vector. Which traces should be modified?
#' @param evaluate logical. Evaluate arguments when this function is called?
#' @seealso \code{\link{get_figure}()}
#' @author Carson Sievert
#' @export
#'
style <- function(p = get_plot(strict = FALSE), ..., traces = 1, evaluate = FALSE) {
  p <- get_plot(p)
  idx <- traces >= length(p$data)
  if (any(idx)) warning("You've referenced non-existent traces", call. = FALSE)
  style <- list(
    args = substitute(list(...)),
    # not optimal....
    env = p$data[[max(traces)]]$env,
    enclos = parent.frame(),
    traces = traces
  )
  p$style <- c(p$style, list(style))
  if (evaluate) p <- plotly_build(p)
  hash_plot(data, p)
}

#' Obtain underlying data of plotly object
#' 
#' Given a data frame with a class of plotly, this function returns the arguments
#' and/or data used to create the plotly. If no data frame is provided, 
#' the last plotly object created in this R session is returned (if it exists).
#' 
#' @param data a data frame with a class of plotly (and a plotly_hash attribute).
#' @export
get_plot <- function(data = NULL, strict = TRUE) {
  hash <- attr(data, "plotly_hash")
  if (!is.null(hash)) {
    get(hash, envir = plotlyEnv)
  } else if (is.data.frame(data)) {
    # safe to just grab the most recent environment?
    hash <- rev(ls(plotlyEnv))[1]
    plotlyEnv[[hash]]
  } else {
    data
  }
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
