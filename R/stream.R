#' Stream objects
#' 
#' @param p a plotly or figure object.
#' @examples
#' 
#' # generate 100 random observations
#' n <- 100
#' x <- rnorm(n)
#' 
#' # start a plotly stream
#' s <- stream()
#' # write to the stream 
#' for (i in seq_len(50)) {
#'   s$write(x = x)
#'   x <- x[-n]
#'   x[1] <- rnorm(1)
#' }
#' # close the stream
#' s$close()
#' 

# Implementation of https://plot.ly/streaming/
stream <- function(x) {
  haz <- has_stream()
  if (!haz) {
    stop("To use plotly streams, you need to create streaming token(s). \n",
         "To see how to create token(s), please visit: \n", 
         "https://plot.ly/python/streaming-tutorial/#Get-your-stream-tokens \n",
         "Once created, save these token(s) as an environment variable: \n",
         "  Sys.setenv('plotly_streamtoken' = 'token1;token2;token3')")
  }
  # obtain streaming tokens
  toks <- strsplit(names(haz), ";")[[1]]
  if (length(toks)) {
    warning("Multiple token handling coming soon!")
    toks <- toks[1]
  }
  # request headers
  headerz <- httr::add_headers(.headers = c(
    "Transfer-Encoding" = "chunked",
    "plotly-streamtoken" = toks
  ))
  # request body
  st <- list(stream = list(token = toks, maxpoints = 500))
  resp <- httr::POST(get_domain("stream"), headerz)
  httr::stop_for_status(resp)
  
  
  list(
    write = function() {},
    close = function() {}
  )
}

has_stream <- function() {
  token <- Sys.getenv("plotly_streamtoken")
  setNames(token != "", token)
}
