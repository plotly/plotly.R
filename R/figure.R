is.figure <- function(x) inherits(x, "figure")

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
#'  fig <- get_figure("cpsievert", "355")
#'  # If you have proper credentials, you can modify it
#'  fig + layout(title = paste("Created on", Sys.Date()))
#' }
get_figure <- function(username, id) {
  if (missing(username)) username <- verify("username")
  if (missing(id)) stop("Must provide a figure id.")
  base_url <- file.path(get_domain(), "apigetfile", username, id)
  resp <- httr::GET(base_url, plotly_headers())
  process(struct(resp, "figure"))
}
