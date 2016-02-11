# ----------------------------------------------------------------------------
# Methods for processing API responses
# ----------------------------------------------------------------------------

process <- function(resp) {
  UseMethod("process")
}

process.clientresp <- function(resp) {
  httr::stop_for_status(resp)
  con <- from_JSON(httr::content(resp, as = "text"))
  # make sure that we always return a HTTPS link
  con$url <- sub("^http[s]?:", "https:", con$url)
  if (nchar(con$error) > 0) stop(con$error, call. = FALSE)
  if (nchar(con$warning) > 0) warning(con$warning, call. = FALSE)
  if (nchar(con$message) > 0) message(con$message, call. = FALSE)
  con
}

process.image <- function(resp) {
  httr::stop_for_status(resp)
  # httr (should) know to call png::readPNG() which returns raster array
  tryCatch(httr::content(resp, as = "parsed"), 
           error = function(e) httr::content(resp, as = "raw"))
}

process.figure <- function(resp) {
  httr::stop_for_status(resp)
  con <- from_JSON(content(resp, as = "text"))
  fig <- con$payload$figure
  fig$url <- sub("apigetfile/", "~", resp$url)
  # make sure that we always return a HTTPS link
  con$url <- sub("^http[s]?:", "https:", con$url)
  fig <- add_boxed(fig)
  fig$data[[1]]$inherit <- FALSE
  # any reasonable way to return a data frame?
  hash_plot(data.frame(), fig)
}

process.signup <- function(resp) {
  httr::stop_for_status(resp)
  con <- from_JSON(content(resp, as = "text"))
  if (nchar(con[["error"]]) > 0) stop(con$error, call. = FALSE)
  # Relaying a message with a private key probably isn't a great idea --
  # https://github.com/ropensci/plotly/pull/217#issuecomment-100381166
  con
}
