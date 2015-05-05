#' Main interface to plotly 
#' 
#' Plotly interface object. See up-to-date documentation and examples at
#' https://plot.ly/API
#' 
#' @param p Either a ggplot object or a list of data/arguments to post to the
#' plotly api.
#' @param browse should the default web browser be prompted to open the Plotly result?
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
  resp <- plotly_POST(p, ...)
  if (browse) browse_url(resp[["url"]])
  resp
}


#' POST messages to plotly's REST API
#' @param args a list. For details see the rest API docs.
#' @param kwargs a list. For details see the rest API docs.
#' @param origin a character vector of length one. For details see the rest API docs.
#' @param ... arguments passed along to \code{httr::POST()}
#' @export
#' @references https://plot.ly/rest/
#' @examples
#' 
#' args <- list(c(0, 1, 2), c(3, 4, 5), c(1, 2, 3), c(6, 6, 5))
#' resp <- plotly_POST(args)
#' 
plotly_POST <- function(args, kwargs = list(filename = "plot from api", fileopt = "new"), 
                        origin = "plot", ...) {
  base_url <- "https://plot.ly/clientresp"
  
  # provide informative error if args/kwargs are missing?
  bod <- list(
    un = verify("username"),
    key = verify("apikey"),
    origin = origin,
    platform = "R",
    version = "0.5.20",
    args = RJSONIO::toJSON(args, digits = 50, collapse = ""),
    kwargs = RJSONIO::toJSON(kwargs, digits = 50, collapse = "")
  )
  resp <- httr::POST(base_url, body = bod, ...)
  stop_for_status(resp)
  cont <- RJSONIO::fromJSON(content(resp, as = "text"))
  if (nchar(cont[["error"]]) > 0) stop(cont[["error"]], call. = FALSE)
  if (nchar(cont[["warning"]]) > 0) warning(cont[["warning"]], call. = FALSE)
  if (nchar(cont[["message"]]) > 0) message(cont[["message"]], call. = FALSE)
  cont
}

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


plotly_embed <- function(into = c("html", "rmd", "notebook")) {
  # TODO
}


# Try to view an 'embedded' version in RStudio preview
browse_url <- function(url) {
  usr <- verify("username")
  id <- sub(".*/([0-9]+)/.*", "\\1", url)
  html <- readLines(system.file("htmljs/index.html", package = "plotly"))
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "index.html")
  html <- gsub("username[/:]id", paste(usr, id, sep = "/"), html)
  writeLines(html, tmp)
  if (requireNamespace("servr")) {
    servr::httd(dirname(tmpdir))
  } else {
    getOption("browser")(tmpdir)
  }
}

is_rstudio <- function() Sys.getenv('RSTUDIO') == '1'



# 
# pub$iplot <- function(..., kwargs = list(filename = NULL, fileopt = NULL)) {
#   # Embed plotly graphs as iframes for knitr documents
#   r <- pub$plotly(..., kwargs = kwargs)
#   # bind url to the knitr options and pass into the plotly knitr hook
#   knit_hooks$set(plotly = function(before, options, envir) {
#     options[["url"]] <- r[["url"]]
#     priv$plotly_hook(before, options, envir)
#   })
# }
# pub$irplot <- function(..., kwargs=list(filename=NULL, fileopt=NULL,
#                                         width=NULL, height=NULL)) {
#   # Embed plotly graphs as iframes in IR notebooks
#   r <- pub$plotly(..., kwargs=kwargs)
#   w <- if (is.null(kwargs$width)) "100%" else kwargs$width
#   h <- if (is.null(kwargs$height)) "525" else kwargs$height
#   html <- paste("<iframe height=\"", h, "\" id=\"igraph\" scrolling=\"no\" seamless=\"seamless\"\n\t\t\t\tsrc=\"", 
#                 r$url, "\" width=\"", w, "\" frameBorder=\"0\"></iframe>", sep="")
#   require(IRdisplay)
#   display_html(html)
# }
# pub$embed <- function(url) {
#   # knitr hook
#   knit_hooks$set(plotly = function(before, options, envir) {
#     options[["url"]] <- url
#     priv$plotly_hook(before, options, envir)
#   })
# }
