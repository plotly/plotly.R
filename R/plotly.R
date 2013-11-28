plotly <-
function(username, key){

	# pub attributes/methods that the user has access to
	pub = list(
		username = username,
		key = key,
		filename='from api',
		fileopt=NULL,
		version = '0.3'
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

	pub$plotly = function(..., kwargs=list(filename=pub$filename,fileopt=NULL)){
		args = list(...)
		return(pub$makecall(args=args,kwargs=kwargs,origin='plot'))
	}

	pub$layout = function(..., kwargs=list(filename=pub$filename,fileopt=NULL)){
		args = list(...)
		return(pub$makecall(args=args,kwargs=kwargs,origin='layout'))
	}
	
	pub$style = function(..., kwargs=list(filename=pub$filename,fileopt=NULL)){
		args = list(...)
		cat(kwargs)
		return(pub$makecall(args=args,kwargs=kwargs,origin='style'))
	}

	## wrap up the object
	pub <- list2env(pub)
	class(pub) <- "PlotlyClass"
	return(pub)
}
