#' Main interface to plotly 
#' 
#' Plotly interface object. See up-to-date documentation and examples at https://plot.ly/API
#' 
#' @description
#' A call to \code{plotly(username, key)} creates an object of class "PlotlyClass", which 
#' has 3 methods:
#' \itemize{
#'  \item Plotting: py$plotly(x1, y1[,x2,y2,...], kwargs=kw) or 
#'    py$plotly({data1[,data2,...]}, kwargs=kwargs) 
#'  \item Styling Data: py$style(data1,data2,..., kwargs=kwargs)
#'  \item Styling Layout: py$layout(layout, kwargs=kwargs)
#' }
#' 
#' @import knitr
#' @param username plotly username
#' @param key plotly API key
#' 
#' @return An object of class PlotlyClass
#' @details See documentation and examples at https://plot.ly/API
#' @references https://plot.ly/API
#' @author Chris Parmer chris@@plot.ly
#' @export
#' @examples \dontrun{
#' ## View https://plot.ly/API for more examples
#' ## Generate a simple plot 
#' username <- "anna.lyst" # fill in with your plotly username
#' api_key <- "y37zkd" # fill in with your plotly API key
#' py <- plotly(username, api_key)
#' ## generate some data
#' x <- c(0,1,2)
#' y <- c(10,11,12)
#' 
#' ## Send data to Plotly. Plotly will render an interactive graph and will return a 
#' ## URL where you can view your plot
#' ## This call sends data to Plotly, Plotly renders an interactive 
#' ##    graph, and returns a URL where you can view your plot
#' response <- py$plot(x,y)
#' url = response$url # view your plot at this URL
#' browseURL(url)
#' }

plotly <- function(username, key){
	# public attributes/methods that the user has access to
	pub = list(
		username = username,
		key = key,
		filename='from api',
		fileopt=NULL,
		version = '0.3.1'
	)

	priv = list()

	pub$makecall = function(args,kwargs,origin){
		if(is.null(kwargs$filename)) kwargs$filename=pub$filename
		if(is.null(kwargs$fileopt)) kwargs$fileopt=NULL
		url = 'https://plot.ly/clientresp'
		options(RCurlOptions = list(sslversion=3, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
		respst = postForm( url, 
								platform="R",
								version=pub$version,
								args=toJSON(args,collapse=''),
								un=pub$username,
								key=pub$key,
								origin=origin,
								kwargs=toJSON(kwargs,collapse=''))
		resp=fromJSON(respst, simplify=FALSE)
		if(!is.null(resp$filename)) pub$filename = resp$filename
		if(!is.null(resp$error)) cat(resp$err)
		if(!is.null(resp$warning)) cat(resp$warning)
		if(!is.null(resp$message)) cat(resp$message)
		return(resp)
	}

	priv$plotly_hook = function(before, options, envir){
		if(!before){
			# set width and height from options or defaults
			if(is.null(options[['width']])) w = '100%'
			else w = options[['width']]
			if(is.null(options[['height']])) h = '600'
			else h = options[['height']]
			paste('<iframe height="',h,'" id="igraph" scrolling="no" seamless="seamless"
				src="', options[['url']],'" width="',w,'"></iframe>',sep='')
		}

	}

	pub$plotly = function(..., kwargs=list(filename=NULL,fileopt=NULL)){
		args = list(...)
		return(pub$makecall(args=args,kwargs=kwargs,origin='plot'))
	}

	pub$iplot = function(..., kwargs=list(filename=NULL, fileopt=NULL)){
		# Embed plotly graphs as iframes for knitr documents
		r = pub$plotly(..., kwargs=kwargs)
		# bind url to the knitr options and pass into the plotly knitr hook
		knit_hooks$set(plotly=function(before,options,envir){
			options[['url']] = r[['url']]
			priv$plotly_hook(before,options,envir)
		})
	}

	pub$embed = function(url){
		# knitr hook
		knit_hooks$set(plotly = function(before,options,envir){
			options[['url']] = url
			priv$plotly_hook(before,options,envir)
		})
	}

	pub$layout = function(..., kwargs=list(filename=NULL,fileopt=NULL)){
		args = list(...)
		return(pub$makecall(args=args,kwargs=kwargs,origin='layout'))
	}
	
	pub$style = function(..., kwargs=list(filename=NULL,fileopt=NULL)){
		args = list(...)
		cat(kwargs)
		return(pub$makecall(args=args,kwargs=kwargs,origin='style'))
	}

	## wrap up the object
	pub <- list2env(pub)
	class(pub) <- "PlotlyClass"
	return(pub)
}