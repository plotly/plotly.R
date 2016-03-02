#' Request a figure object
#' 
#' Figure objects work in the same way as plotly objects, but when printed,
#' they overwrite the already existing plotly object 
#' (instead of creating a new plotly).
#' 
#' @param username corresponding username for the figure.
#' @param id of the Plotly figure.
#' @export
#' @references https://plot.ly/rest/
#' @examples
#' \dontrun{
#'  # Anyone can obtain the information for a particular plot
#'  fig <- get_figure("cpsievert", "559")
#'  # If you have proper credentials, you can easily modify
#'  layout(fig, title = paste("Modified on ", Sys.time()))
#' }
get_figure <- function(username, id) {
  if (missing(username)) username <- verify("username")
  if (missing(id)) stop("Please provide a figure id number")
  base_url <- file.path(get_domain(), "apigetfile", username, id)
  resp <- httr::GET(base_url, plotly_headers(), httr::config(ssl_verifypeer=FALSE))
  process(append_class(resp, "figure"))
}
