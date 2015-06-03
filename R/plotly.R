is.plotly <- function(x) inherits(x, "plotly")

#' Method for adding together plotly objects
#' 
#' @param x a plotly object 
#' @param y another object
#' @author Carson Sievert
#' @export
"+.plotly" <- function(x, y) {
  if (is.ggplot(y)) 
    stop("Don't know how to add ggplot object(s) to a plotly object(s)",
         "Try converting your ggplot object to a plotly object with ggplotly()")
  if (is.aux(y))
    stop("An auxiliary object must be provided as an argument to a trace object")
  if (sum(is.style(y), is.layoutLike(y), is.layout(y), is.plotly(y)) == 0)
    stop("Don't know how to add ", deparse(substitute(y)), " to a plotly object")
  
  if (is.style(y)) {
    ntraces <- length(x$data)
    if (any(y$traces > ntraces))
      warning("Your style object references non-existent traces")
    z <- y[!grepl("traces", names(y))]
    for (i in y$traces) {
      x$data[[i]] <- modifyList(x$data[[i]], z)
    }
    x
  } else if (is.layoutLike(y)) {
    # first class of a layout object should always correspond to it's 'layout link'
    z <- class(y)[1]
    y <- setNames(list(y), z)
    modifyList(x, list(layout = y))
  } else if (is.layout(y)) {
    modifyList(x, list(layout = y))
  } else if (is.plotly(y)) {
    x$data <- c(x$data, y$data)
    structure(x, class = unique(class(x), class(y)))
  }
}

#' Method for adding together plotly objects
#' 
#' @param x a figure object 
#' @param y another object
#' @author Carson Sievert
#' @export
"+.figure" <- `+.plotly`

# TODO: Does a `+` method for _overriding_ the LHS make any sense? Maybe for layout?


#' Main interface to plotly 
#' 
#' Deprecated: see \link{signup} for credentials/configuration storage details.
#' See \link{ggplotly} for the new ggplot2 interface.
#' 
#' @param username plotly username
#' @param key plotly API key
#' @param base_url plotly server
#' @export
plotly <- function(username = NULL, key = NULL, base_url = NULL) {
  if (!missing(username)) {
    message("Storing API key as the environment variable 'plotly_username'")
    Sys.setenv("plotly_username" = username)
  }
  if (!missing(key)) {
    message("Storing API key as the environment variable 'plotly_api_key'")
    Sys.setenv("plotly_api_key" = key)
  } else {
    .Deprecated("signup")
  }
  .Deprecated("ggplotly")
  invisible(NULL)
}
