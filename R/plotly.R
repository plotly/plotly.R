#' Main interface to plotly 
#' 
#' Plotly interface object. See up-to-date documentation and examples at
#' https://plot.ly/API
#' 
#' @description
#' A call to \code{plotly(username, key)} creates an object of class
#' 'PlotlyClass', which has methods:
#' \itemize{
#'  \item Plotting: py$plotly(x1, y1[, x2, y2, ...], kwargs=kwargs) or
#'    py$plotly({data1[, data2, ...]}, kwargs=kwargs), py$ggplotly()
#'  \item Styling Data: py$style(data1,data2,..., kwargs=kwargs)
#'  \item Styling Layout: py$layout(layout, kwargs=kwargs)
#'  \item Utilities: py$get_figure(file_owner, file_id)
#' }
#' 
#' @import knitr
#' @import RJSONIO
#' @param username plotly username
#' @param key plotly API key
#' 
#' @return An object of class PlotlyClass, except for the final object after
#' adding layers becomes a list class.
#' @details See documentation and examples at https://plot.ly/API
#' @references https://plot.ly/API
#' @author Chris Parmer chris@@plot.ly
#' @export
#' @examples \dontrun{
#' ## View https://plot.ly/API for more examples
#' ## Generate a simple plot
#' username <- 'anna.lyst'  # fill in with your plotly username
#' api_key <- 'y37zkd'  # fill in with your plotly API key
#' py <- plotly(username, api_key)
#' ## generate some data
#' x <- c(0, 1, 2)
#' y <- c(10, 11, 12)
#' 
#' ## Send data to Plotly. Plotly will render an interactive graph and will
#' ## return a URL where you can view your plot
#' ## This call sends data to Plotly, Plotly renders an interactive 
#' ## graph, and returns a URL where you can view your plot
#' response <- py$plot(x, y)
#' response$url  # view your plot at this URL
#' browseURL(response$url)  # use browseURL to go to the URL in your browser
#'
#' ## Export ggplots directly to plot.ly
#' ggiris <- qplot(Petal.Width, Sepal.Length, data=iris, color=Species)
#' py$ggplotly(ggiris)
#' data(canada.cities, package="maps")
#' viz <- ggplot(canada.cities, aes(long, lat)) +
#'   borders(regions="canada", name="borders") +
#'   coord_equal() +
#'   geom_point(aes(text=name, size=pop), colour="red",
#'                alpha=1/2, name="cities")
#'  py$ggplotly(viz)
#' }


plotly <- function(username=NULL, key=NULL) {
  
  if (is.null(username)) {
    username <- get_credentials_file(c("username", "api_key"))$username
  }
  if (is.null(key)) {
    key <- get_credentials_file(c("username", "api_key"))$api_key
  }
  if (is.null(username) || username == "" || is.null(key) || key == "") {
    stop("Credentials Not Found!\n
It looks like you haven't set up your Plotly account credentials yet.\n
To get started, save your plotly username and API key by calling:\n
> set_credentials_file(UserName, ApiKey)\n
For more help, see https://plot.ly/R or contact <chris@plot.ly>.")
  }
  
  # public attributes/methods that the user has access to
  pub <- list(username=username, key=key, filename="from api", fileopt=NULL,
              version="0.4.0")
  priv <- list()
  
  pub$makecall <- function(args, kwargs, origin) {
    if (is.null(kwargs$filename))
      kwargs$filename <- pub$filename
    if (is.null(kwargs$fileopt))
      kwargs$fileopt <- NULL
    url <- "https://plot.ly/clientresp"
    options(RCurlOptions=list(sslversion=3,
                              cainfo=system.file("CurlSSL", "cacert.pem",
                                                 package="RCurl")))
    respst <- postForm(url, platform="R", version=pub$version,
                       args=toJSON(args, collapse=""), un=pub$username,
                       key=pub$key, origin=origin,
                       kwargs=toJSON(kwargs, collapse=""))
    resp <- fromJSON(respst, simplify = FALSE)
    if (!is.null(resp$filename))
      pub$filename <- resp$filename
    if (!is.null(resp$error))
      cat(resp$err)
    if (!is.null(resp$warning))
      cat(resp$warning)
    if (!is.null(resp$message))
      cat(resp$message)
    return(resp)
  }
  priv$plotly_hook <- function(before, options, envir) {
    if (!before) {
      # set width and height from options or default square
      w <- if(is.null(options[["width"]])) "600" else options[["width"]]
      h <- if(is.null(options[["height"]])) "600" else options[["height"]]
      paste("<iframe height=\"", h,
            "\" id=\"igraph\" scrolling=\"no\" seamless=\"seamless\"\n\t\t\t\tsrc=\"",
            options[["url"]], "\" width=\"", w,
            "\" frameBorder=\"0\"></iframe>", sep="")
    }
  }
  pub$plotly <- function(..., kwargs = list(filename = NULL, fileopt = NULL)) {
    args <- list(...)
    return(pub$makecall(args = args, kwargs = kwargs, origin = "plot"))
  }
  pub$ggplotly <- function(gg=last_plot()){
    if(!is.ggplot(gg)){
      stop("gg must be a ggplot")
    }
    pargs <- gg2list(gg)
    if(interactive()){ # we are on the command line.
      resp <- do.call(pub$plotly, pargs)
      browseURL(resp$url)
      invisible(list(data=pargs, response=resp))
    }else{ # we are in knitr/RStudio.
      do.call(pub$iplot, pargs)
    }
  }
  pub$get_figure <- function(file_owner, file_id) {
    headers <- c("plotly-username"=pub$username,
                 "plotly-apikey"=pub$key,
                 "plotly-version"=pub$version,
                 "plotly-platform"="R")
    response_handler <- basicTextGatherer()
    header_handler <- basicTextGatherer()
    curlPerform(url=paste("https://plot.ly/apigetfile", file_owner, file_id,
                          sep="/"),
                httpheader=headers,
                writefunction=response_handler$update,
                headerfunction=header_handler$update)
    resp_header <- as.list(parseHTTPHeader(header_handler$value()))
    
    # Parse status
    if (resp_header$status != "200") {
      cat(resp_header$statusMsg)
      stop(resp_header$status)
    }
    
    body_string <- response_handler$value()
    resp <- RJSONIO::fromJSON(body_string)
    if (!is.null(resp$error) && resp$error != "")
      stop(resp$err)
    if (!is.null(resp$warning) && resp$error != "")
      cat(resp$warning)
    if (!is.null(resp$message) && resp$error != "")
      cat(resp$message)
    
    resp$payload$figure
  }
  pub$iplot <- function(..., kwargs = list(filename = NULL, fileopt = NULL)) {
    # Embed plotly graphs as iframes for knitr documents
    r <- pub$plotly(..., kwargs = kwargs)
    # bind url to the knitr options and pass into the plotly knitr hook
    knit_hooks$set(plotly = function(before, options, envir) {
      options[["url"]] <- r[["url"]]
      priv$plotly_hook(before, options, envir)
    })
  }
  pub$embed <- function(url) {
    # knitr hook
    knit_hooks$set(plotly = function(before, options, envir) {
      options[["url"]] <- url
      priv$plotly_hook(before, options, envir)
    })
  }
  pub$layout <- function(..., kwargs = list(filename = NULL, fileopt = NULL)) {
    args <- list(...)
    return(pub$makecall(args = args, kwargs = kwargs, origin = "layout"))
  }
  pub$style <- function(..., kwargs = list(filename = NULL, fileopt = NULL)) {
    args <- list(...)
    cat(kwargs)
    return(pub$makecall(args = args, kwargs = kwargs, origin = "style"))
  }
  ## wrap up the object
  pub <- list2env(pub)
  class(pub) <- "PlotlyClass"
  return(pub)
}
