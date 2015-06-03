#' Create/Modify plotly graphs
#'
#' POST messages to the clientresp resource of plotly's REST API. Unlike \link{ggplotly},
#' this function does not translate ggplot objects.
#'
#' @param x a list.
#' @export
#' @references https://plot.ly/rest/
#' @seealso \link{signup}
#' @return An R object created by mapping the JSON content of the plotly API
#' response to its R equivalent.
#' @author Carson Sievert
#' @examples
#' \dontrun{
#' # If you want, you can still construct lists by hand...
#' trace1 <- list(
#'   x = c(1, 2, 3, 4), 
#'   y = c(10, 15, 13, 17), 
#'   type = "scatter"
#' )
#' trace2 <- list(
#'   x = c(1, 2, 3, 4), 
#'   y = c(16, 5, 11, 9), 
#'   type = "scatter"
#' )
#' plotly_POST(list(trace1, trace2))
#' }
#'
#'

plotly_POST <- function(x) {
  if (!is.list(x)) stop("x must be a list")
  nms <- names(x)
  # initialize positional and keyword 'arguments' as empty lists
  args <- kwargs <- list()
  if (is.null(nms)) {
    args <- x
  } else {
    args <- x$data
    args <- c(args, x[nms %in% ""])
    idx <- nms %in% get_kwargs()
    # TODO: throw warning/error if we detect names that aren't recognized?
    kwargs <- x[idx]
  }
  # filename & fileopt are keyword arguments required by the API
  # (note they can also be specified by the user)
  if (is.null(kwargs$filename)) kwargs$filename <- "plot from api"
  if (is.null(kwargs$fileopt)) 
    # figure objects should be overwritten when POSTED
    kwargs$fileopt <- if (is.figure(x)) "overwrite" else "new"
  
  # layout can be an empty list
  kwargs <- kwargs[sapply(kwargs, length) > 0]
  
  # construct body of message to plotly server
  bod <- list(
    un = verify("username"),
    key = verify("api_key"),
    origin = if (is.null(x$origin)) "plot" else x$origin,
    platform = "R",
    version = as.character(packageVersion("plotly")),
    args = to_JSON(args),
    kwargs = to_JSON(kwargs)
  )
  base_url <- file.path(get_domain(), "clientresp")
  resp <- httr::POST(base_url, body = bod)
  con <- process(struct(resp, "clientresp"))
  attr(x, "url") <- con$url
  attr(x, "filename") <- con$filename
  x
}
