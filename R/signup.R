signup <-
function(username,email){
	platform = 'R'
	version = '0.3'
	url = 'https://plot.ly/apimkacct'
	options(RCurlOptions = list(sslversion=3, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
	respst = postForm( url,
					platform=platform,
					version=version,
					email=email,
					un=username)
	resp=fromJSON(respst, simplify=FALSE)	
	if(!is.null(resp$filename)) pub$filename = resp$filename
	if(!is.null(resp$error)) cat(resp$err)
	if(!is.null(resp$warning)) cat(resp$warning)
	if(!is.null(resp$message)) cat(resp$message)
	return(resp)
}
