#' Create/Modify plotly graphs
#'
#' Create and modify graphs on your plotly account via plotly's REST API
#' \url{https://plot.ly/rest/}
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
#' @seealso \link{plot_ly}, \link{signup}
#' @return An R object created by mapping the JSON content of the plotly API
#' response to its R equivalent.
#' @author Carson Sievert
#' @examples
#' \dontrun{
#' p <- plot_ly(mtcars, x = vs, type = "bar")
#' plotly_POST(p, filename = "mtcars-bar-plot")
#' }

plotly_POST <- function(x, filename = NULL, fileopt = "overwrite", 
                        sharing = c("public", "private", "secret")) {
  x <- plotly_build(x)
  # try our damndest to assign a sensible filename
  x$filename <- filename %||% x$filename %||% as.character(x$layout$title) %||% 
      paste(c(x$layout$xaxis$title, x$layout$yaxis$title, x$layout$zaxis$title), 
            collapse = " vs. ") %||% paste("Created at", Sys.time())
  if (!is.null(x$fileopt)) {
    warning("fileopt was specified in the wrong place.",
            "Please specify in plotly_POST()")
  }
  x$fileopt <- fileopt
  if (!is.null(x$world_readable)) {
    warning('world_readable is no longer supported. Instead, set the sharing\n',
            'argument to "private" (you must be logged in to access),\n',
            '"secret" (anybody with the obscured URL can access) or "public"\n', 
            '(anybody can view).')
  }
  x$world_readable <- if (sharing[1] == "public") TRUE else FALSE
  
  # plotly server has trouble with empty properties
  x <- x[sapply(x, length) > 0]
  # construct body of message to plotly server
  bod <- list(
    un = verify("username"),
    key = verify("api_key"),
    origin = "plot",
    platform = "R",
    version = as.character(packageVersion("plotly")),
    args = to_JSON(compact(x$data)),
    kwargs = to_JSON(compact(x[get_kwargs()]))
  )
  base_url <- file.path(get_domain(), "clientresp")
  resp <- httr::POST(base_url, body = bod)
  con <- process(append_class(resp, "clientresp"))
  if (sharing[1] == "secret") {
    bits <- strsplit(con$url, "/")[[1]]
    plot_id <- bits[length(bits)]
    url <- file.path(get_domain("api"), "v2", "files", 
                     paste0(verify("username"), ":", plot_id))
    bod <- list(share_key_enabled = TRUE)
    con2 <- httr::PATCH(url, plotly_headers("v2"), body = bod, encode = "json")
    con$url <- paste0(con$url, "?share_key=", content(con2)$share_key)
  }
  msg <- switch(x$fileopt %||% "new",
                new = "Success! Created a new plotly here -> ",
                overwrite = "Success! Modified your plotly here -> ")
  message(msg, con$url)
  structure(con, class = "figure")
}
