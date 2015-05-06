#' Create a new plotly account.
#'
#' A sign up interface to plotly through the R Console.
#'
#' @param username Desired username.
#' @param email Desired email.
#' @param save If request is successful, should the username & API key be
#' automatically stored as an environment variable in a .Rprofile?
#'
#' @return
#' \itemize{
#'  \item api_key key to use with the api
#'  \item tmp_pw temporary password to access your plotly account
#' }
#' @references https://plot.ly/rest/
#' @export
signup <- function(username, email, save = TRUE) {
  if (missing(username)) username <- verify("username")
  if (missing(email)) stop("Must specify a valid email")
  # construct body of message to plotly server
  bod <- list(
    un = username,
    email = email,
    platform = "R",
    version = as.character(packageVersion("plotly"))
  )
  base_url <- file.path(get_domain(), "apimkacct")
  resp <- httr::POST(base_url, body = bod)
  stop_for_status(resp)
  con <- RJSONIO::fromJSON(content(resp, as = "text"))
  # TODO: alter the API response messages to reflect the changes in 1.0.0
  if (nchar(con[["error"]]) > 0) stop(con[["error"]], call. = FALSE)
  if (nchar(con[["message"]]) > 0) message(con[["message"]], call. = FALSE)
  # store API key as an environment variable in .Rprofile
  if (save) {
    cat_profile("username", con[["un"]])
    cat_profile("apikey", con[["api_key"]])
  }
  structure(con, class = "apimkacct")
}

#' Create, modify and style plotly graphs from R
#'
#' Create, See up-to-date documentation and examples at
#' https://plot.ly/API
#'
#' @param p Either a ggplot object or a list of data/arguments to post to the
#' plotly API.
#' @param browse should the default web browser be prompted to open the Plotly result?
#' @param ... additional arguments passed onto \link{plotly_POST}.
#' @seealso \link{signup}, \link{plotly_POST}
#' @import httr RJSONIO
#' @export
#' @examples \dontrun{
#' # You need a plotly username and API key to communicate with the plotly API.
#'
#' # If you don't already have an API key, you can obtain one with a valid
#' # username and email via signup().
#' s <- signup('anna.lyst', 'anna.lyst@@plot.ly')
#'
#' # If you already have a username and API key, please create the following
#' # environment variables:
#' Sys.setenv(`plotly-username` = "me")
#' Sys.setenv(`plotly-apikey` = "mykey")
#' # You can also change the default domain if you have a plotly server.
#' Sys.setenv(`plotly-domain` = "http://mydomain.com")
#'
#' # If you don't want to specify these environment variables everytime you
#' # start R, you can put that code in a .Rprofile (see help(.Rprofile))
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
  # In an effort to save some legacy users headache...
  # specifying username and key should still work
  .args <- as.list(match.call())
  if ("username" %in% names(.args))
    Sys.setenv(`plotly-username` = args[["username"]])
  if ("key" %in% names(.args))
    Sys.setenv(`plotly-apikey` = args[["key"]])
  if (!"data" %in% names(p))
    stop("p should have at least one element named 'data'",
         "(which is mapped to the args parameter in the plotly REST API).")
  resp <- plotly_POST(p$data, list(layout = p$layout), ...)
  if (browse) browseURL(resp[["url"]])
  resp
}

#' Create, modify and style plotly graphs from R
#'
#' POST messages to the clientresp resource of plotly's REST API. Unlike \link{plotly},
#' this function does not support ggplot objects.
#'
#' @param args a list. For details see the rest API docs.
#' @param kwargs a list. For details see the rest API docs.
#' @param origin a character vector of length one. For details see the rest API docs.
#' @param ... arguments passed along to \code{httr::POST()}
#' @export
#' @references https://plot.ly/rest/
#' @seealso \link{signup}, \link{plotly}
#' @return An R object created by mapping the JSON content of the plotly API
#' response to its R equivalent. This object has a class of "clientresp"
#' @examples
#'
#' args <- list(c(0, 1, 2), c(3, 4, 5), c(1, 2, 3), c(6, 6, 5))
#' resp <- plotly_POST(args)
#'
#' # translate a ggplot object with gg2list(), then upload to plotly
#' p <- gg2list(qplot(1:10))
#' resp <- plotly_POST(p$data, list(layout = p$layout), ...)
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
    version = as.character(packageVersion("plotly")),
    args = RJSONIO::toJSON(args, digits = 50, collapse = ""),
    kwargs = RJSONIO::toJSON(kwargs, digits = 50, collapse = "")
  )
  base_url <- file.path(get_domain(), "clientresp")
  resp <- httr::POST(base_url, body = bod, ...)
  stop_for_status(resp)
  con <- RJSONIO::fromJSON(content(resp, as = "text"))
  if (nchar(con[["error"]]) > 0) stop(con[["error"]], call. = FALSE)
  if (nchar(con[["warning"]]) > 0) warning(con[["warning"]], call. = FALSE)
  if (nchar(con[["message"]]) > 0) message(con[["message"]], call. = FALSE)
  structure(con, class = "clientresp")
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
knit_print.clientresp <- function(x, options, ...) {
  if (!requireNamespace("knitr")) {
    warning("Please install.packages('knitr')")
    return(x)
  }
  w <- if (is.null(options[["width"]])) "600" else options[["width"]]
  h <- if (is.null(options[["height"]])) "600" else options[["height"]]
  iframe <- plotly_iframe(x[["url"]], h, w)
  knitr::asis_output(iframe)
}

#' Embed a plotly iframe into a IPython Notebook
#' @param url A url pointing to a plotly graph
#' @param width attribute of the iframe
#' @param height attribute of the iframe
#' @export
embed_notebook <- function(url, width = "100%", height = "525") {
  if (!inherits(p, "clientresp")) {
    p <- plotly(p)
    url <- p[["url"]]
  }
  if (!requireNamespace("IRdisplay")) {
    warning("You need the IRdisplay package to use this function: \n",
            "devtools::install_github(c('IRkernel/repr', 'IRKernel/IRdisplay'))")
    return(p)
  }
  IRdisplay::display_html(plotly_iframe(url, height, width))
}

# ----------------------------------------
# Non-exported helper functions
# ----------------------------------------

get_domain <- function() {
  Sys.getenv("plotly-domain", "https://plot.ly")
}

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

# try to write environment variables to an .Rprofile
cat_profile <- function(key, value, path = "~") {
  r_profile <- file.path(normalizePath(path, mustWork = TRUE),
                         ".Rprofile")
  snippet <- sprintf('\nSys.setenv(`plotly-%s` = "%s")', key, value)
  if (!file.exists(r_profile)) {
    message("Creating", r_profile)
    r_profile_con <- file(r_profile)
  }
  if (file.access(r_profile, 2) != 0)
    stop("R doesn't have permission to write to this file: ", path)
  if (file.access(r_profile, 4) != 0)
    stop("R doesn't have permission to read this file: ", path)
  message("Adding plotly-", key, " environment variable to ", r_profile)
  cat(snippet, file = r_profile, append = TRUE)
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
