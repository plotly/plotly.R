#' Create a new Plotly account.
#'
#' A sign up interface to Plotly through the R Console.
#'
#' @param username Desired username
#' @param email Desired email
#'
#' @return
#' \itemize{
#'  \item api_key key to use with the api
#'  \item tmp_pw temporary password to access your plotly account
#' }
#' @references https://plot.ly/rest/
#' @export
signup <- function(username, email) {
  if (missing(username)) username <- verify("username")
  if (missing(email)) stop("Must specify a valid email")
  base_url <- "https://plot.ly/apimkacct"
  bod <- list(
    un = username,
    email = email,
    platform = "R",
    version = as.character(packageVersion("plotly"))
  )
  resp <- httr::POST(base_url, body = bod)
  stop_for_status(resp)
  RJSONIO::fromJSON(content(resp, as = "text"))
}

#' Main interface to plotly
#'
#' Plotly interface object. See up-to-date documentation and examples at
#' https://plot.ly/API
#'
#' @param p Either a ggplot object or a list of data/arguments to post to the
#' plotly api.
#' @param browse should the default web browser be prompted to open the Plotly result?
#' @param ... additional arguments passed onto \code{plotly_POST}.
#' @references https://plot.ly/API
#' @import httr RJSONIO
#' @export
#' @examples \dontrun{
#' # You need a plotly username and API key to communicate with
#' # the plotly API. These are accessed via environment variables.
#' # If you don't already have an API key, you can obtain one with a valid
#' # username and email via the signup() function.
#' usr <- 'anna.lyst'
#' Sys.setenv(`plotly-username` = usr)
#' resp <- signup(usr, 'anna.lyst@@plot.ly')
#' Sys.setenv(`plotly-apikey` = resp[["apikey"]])
#' # Note that you can set environment variables in your .Rprofile if you
#' # don't want to set them everytime you start R.
#'
#' # Send data directly to Plotly's Javascript Graphing Library
#' # https://plot.ly/javascript-graphing-library/
#' p <- list(
#'  x = c(0, 1, 2),
#'  y = c(10, 11, 12)
#' )
#' resp <- plotly(p)
#'
#' # plotly() also understands how to map (some) ggplot objects to Plotly graphs
#' ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)
#' plotly(ggiris)
#' data(canada.cities, package="maps")
#' viz <- ggplot(canada.cities, aes(long, lat)) +
#'   borders(regions="canada", name="borders") +
#'   coord_equal() +
#'   geom_point(aes(text=name, size=pop), colour="red",
#'                alpha=1/2, name="cities")
#'  plotly(viz)
#' }
plotly <- function(p = last_plot(), browse = interactive(), ...) {
  if (is.ggplot(p)) {
    p <- gg2list(p)
  } else if (!is.list(p)) {
    stop("p must be either a ggplot object or a list")
  }
  # how to best map list to a post message?
  resp <- plotly_POST(p$data, list(layout = p$layout), ...)
  if (browse) browseURL(resp[["url"]])
  resp
}

#' POST messages to plotly's REST API
#' @param args a list. For details see the rest API docs.
#' @param kwargs a list. For details see the rest API docs.
#' @param origin a character vector of length one. For details see the rest API docs.
#' @param ... arguments passed along to \code{httr::POST()}
#' @export
#' @references https://plot.ly/rest/
#' @return An R object created by mapping the JSON content of the plotly API
#' response to its R equivalent. This object has a class of "plotly_response"
#' @examples
#'
#' args <- list(c(0, 1, 2), c(3, 4, 5), c(1, 2, 3), c(6, 6, 5))
#' resp <- plotly_POST(args)
#'
plotly_POST <- function(args, kwargs = list(filename = "plot from api", fileopt = "new"),
                        origin = "plot", ...) {
  # some basic input checks
  if (!is.list(args)) stop("args must be a list")
  if (!is.list(kwargs)) stop("kwargs must be a list")
  nms <- names(kwargs)
  # filename and fileopt are required
  if (!"filename" %in% nms) kwargs$filename <- "plot from api"
  if (!"fileopt" %in% nms) kwargs$fileopt <- "new"
  # construct body of message to plotly server
  bod <- list(
    un = verify("username"),
    key = verify("apikey"),
    origin = origin,
    platform = "R",
    version = "0.5.20",
    args = RJSONIO::toJSON(args, digits = 50, collapse = ""),
    kwargs = RJSONIO::toJSON(kwargs, digits = 50, collapse = "")
  )
  # TODO: support different plotly domains?
  resp <- httr::POST("https://plot.ly/clientresp", body = bod, ...)
  stop_for_status(resp)
  cont <- RJSONIO::fromJSON(content(resp, as = "text"))
  if (nchar(cont[["error"]]) > 0) stop(cont[["error"]], call. = FALSE)
  if (nchar(cont[["warning"]]) > 0) warning(cont[["warning"]], call. = FALSE)
  if (nchar(cont[["message"]]) > 0) message(cont[["message"]], call. = FALSE)
  structure(cont, class = "plotly_response")
}

#' Request data/layout for a particular Plotly figure
#' @param username corresponding username for the figure.
#' @param id of the Plotly figure.
#' @export
#' @references https://plot.ly/rest/
#' @examples
#'
#' # https://plot.ly/~TestBot/100
#' resp <- get_figure("TestBot", "100")
#' names(resp[["layout"]])
#' names(resp[["data"]])
get_figure <- function(username, id) {
  base_url <- file.path("https://plot.ly/apigetfile", username, id)
  resp <- httr::GET(base_url, plotly_headers())
  stop_for_status(resp)
  RJSONIO::fromJSON(content(resp, as = "text"))[["payload"]][["figure"]]
}

#' Embed a plotly iframe into an R markdown document via \code{knit_print}
#' @param x named list of ggplots and option lists to pass to \code{animint2dir}.
#' @param options knitr options.
#' @param ... placeholder.
#' @export
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
knit_print.plotly_response <- function(x, options, ...) {
  if (!requireNamespace("knitr")) warning("Please install.packages('knitr')")
  w <- if (is.null(options[["width"]])) "600" else options[["width"]]
  h <- if (is.null(options[["height"]])) "600" else options[["height"]]
  plotly_iframe(x[["url"]], h, w)
}

#' Embed a plotly iframe into a IPython Notebook
#' @param url A url pointing to a plotly graph
#' @param width attribute of the iframe
#' @param height attribute of the iframe
#' @export
embed_notebook <- function(url, width = "100%", height = "525") {
  if (!inherits(p, "plotly_response")) {
    p <- plotly(p)
    url <- p[["url"]]
  }
  if (!requireNamespace("IRdisplay")) {
    message("You need the IRdisplay package to use this function: \n",
            "devtools::install_github(c('IRkernel/repr', 'IRKernel/IRdisplay'))")
    return(p)
  }
  IRdisplay::display_html(plotly_iframe(url, height, width))
}

# ----------------------------------------
# Non-exported helper functions
# ----------------------------------------

plotly_headers <- function() {
  httr::add_headers(.headers = c(
                    "plotly-username" = verify("username"),
                    "plotly-apikey" = verify("apikey"),
                    "plotly-version" = as.character(packageVersion("plotly")),
                    "plotly-platform" = "R"))
}

# verify that a certain environment variable exists
verify <- function(what = "username") {
  who <- paste0("plotly-", what)
  val <- Sys.getenv(who, "")
  if (val == "") stop("Must specify ", what, call. = FALSE)
  val
}

plotly_iframe <- function(url, width, height) {
  paste("<iframe height=\"", height, "\" id=\"igraph\" scrolling=\"no\" seamless=\"seamless\"\n\t\t\t\tsrc=\"",
        url, "\" width=\"", width, "\" frameBorder=\"0\"></iframe>", sep="")
}

# bummer, looks like we can't use RStudio's viewer (yet) --
# https://github.com/rstudio/rstudioapi/issues/2#issuecomment-99250180
# browse_url <- function(url) {
#   usr <- verify("username")
#   id <- sub(".*/([0-9]+)[/]?.*", "\\1", url)
#   html <- readLines(system.file("htmljs/index.html", package = "plotly"))
#   tmp <- tempfile(fileext = ".html")
#   html <- gsub("username/id", paste(usr, id, sep = "/"), html)
#   writeLines(html, tmp)
#   # Try to view an 'embedded' version in RStudio preview. This was
#   # copied/adapted from Yihui Xie's work on servr --
#   # https://github.com/yihui/servr/blob/39a61972e278adc5bbd49a74c68de858bb2c144f/R/utils.R#L55-L69
#   browseR = if ('tools:rstudio' %in% search()) getOption('viewer') else {
#     if (is_rstudio()) getFromNamespace('viewer', 'rstudioapi')
#   }
#   # rstudio::viewer() does not seem to work when a separate R session is
#   # launched from RStudio, so we need to try() and if it fails, fall back to the
#   # default web browser
#   if (is.null(browseR) || !is.function(browseR) ||
#       inherits(try(browseR('http://www.rstudio.com'), silent = TRUE), 'try-error'))
#     browseR = getOption('browser')
#   browseR(tmp)
# }
#
# is_rstudio <- function() Sys.getenv('RSTUDIO') == '1'
