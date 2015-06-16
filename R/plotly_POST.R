#' Create/Modify plotly graphs
#'
#' POST messages to the clientresp resource of plotly's REST API. Unlike \link{ggplotly},
#' this function does not translate ggplot objects.
#'
#' @param x a list or an environment.
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

plotly_POST <- function(x) {
  x <- eval_list(x)
  args <- x$data
  kwargs <- x[get_kwargs()]
  
  # search for keyword args in traces and take the first valid one
  kwargs2 <- lapply(x$data, function(x) x[get_kwargs()])
  kwargs <- modifyList(kwargs, Reduce(c, kwargs2))
  
  # filename & fileopt are keyword arguments required by the API
  # (note they can also be specified by the user)
  if (is.null(kwargs$filename)) kwargs$filename <- "plot from api"
  if (is.null(kwargs$fileopt)) kwargs$fileopt <- "new"
  
  # layout can be an empty list
  kwargs <- kwargs[sapply(kwargs, length) > 0]
  
  # construct body of message to plotly server
  bod <- list(
    un = verify("username"),
    key = verify("api_key"),
    origin = if (is.null(x$origin)) "plot" else x$origin,
    platform = "R",
    version = as.character(packageVersion("plotly")),
    args = to_JSON(setNames(args, NULL)),
    kwargs = to_JSON(kwargs)
  )
  base_url <- file.path(get_domain(), "clientresp")
  resp <- httr::POST(base_url, body = bod)
  con <- process(struct(resp, "clientresp"))
  msg <- switch(kwargs$fileopt,
                new = "Success! Created a new plotly here -> ",
                overwrite = "Success! Modified your plotly here -> ")
  message(msg, con$url)
  con
}
