#' Create/Modify plotly graphs
#'
#' Create and modify graphs on your plotly account via plotly's REST API
#' \url{https://plot.ly/rest/}
#'
#' @param x either a ggplot object, a plotly object, or a list.
#' @param filename character string describing the name of the plot in your plotly account. 
#' Use / to specify directories. If a directory path does not exist it will be created.
#' If this argument is not specified and the title of the plot exists,
#' that will be used for the filename.
#' @param fileopt character string describing whether to create a "new" plotly, "overwrite" an existing plotly, 
#' "append" data to existing plotly, or "extend" it.
#' @param world_readable logical. If \code{TRUE}, the graph is viewable 
#' by anyone who has the link and in the owner's plotly account.
#' If \code{FALSE}, graph is only viewable in the owner's plotly account.
#' @export
#' @seealso \link{plot_ly}, \link{signup}
#' @return An R object created by mapping the JSON content of the plotly API
#' response to its R equivalent.
#' @author Carson Sievert
#' @examples
#' \dontrun{
#' p <- plot_ly(mtcars, x = vs, type = "bar")
#' plotly_POST(p, filename = "mtcars-bar-plot")
#' }

# TODO: support all the API arguments???
plotly_POST <- function(x, filename, fileopt = "new", world_readable = TRUE) {
  x <- plotly_build(x)
  if (!missing(filename)) x$filename <- filename
  if (!is.null(x$fileopt)) 
    warning("fileopt was specified in the wrong place. Please specify in plotly_POST()")
  x$fileopt <- fileopt
  if (!is.null(x$world_readable)) 
    warning("world_readable was specified in the wrong place. Please specify in plotly_POST()")
  x$world_readable <- world_readable
  # construct body of message to plotly server
  bod <- list(
    un = verify("username"),
    key = verify("api_key"),
    origin = "plot",
    platform = "R",
    version = as.character(packageVersion("plotly")),
    args = to_JSON(x$data),
    kwargs = to_JSON(kwargs)
  )
  base_url <- file.path(get_domain(), "clientresp")
  resp <- httr::POST(base_url, body = bod)
  con <- process(struct(resp, "clientresp"))
  msg <- switch(x$fileopt %||% "new",
                new = "Success! Created a new plotly here -> ",
                overwrite = "Success! Modified your plotly here -> ")
  message(msg, con$url)
  structure(con, class = "figure")
}
