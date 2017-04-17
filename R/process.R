# ----------------------------------------------------------------------------
# Methods for processing API responses
# ----------------------------------------------------------------------------

process <- function(resp) {
  UseMethod("process")
}

process.default <- function(resp) {
  json_content(relay_error(resp))
}

process.api_plot <- function(resp) {
  json_content(relay_error(resp))
}

process.api_image <- function(resp) {
  relay_error(resp)
  # httr (should) know to call png::readPNG() which returns raster array
  tryCatch(httr::content(resp, as = "parsed"), 
           error = function(e) httr::content(resp, as = "raw"))
}

# the default for httr::content() doesn't simplify vectors apparently...
json_content <- function(resp) {
  from_JSON(
    httr::content(resp, as = "text")
  )
}

relay_error <- function(resp) {
  if (!httr::http_error(resp)) {
    return(resp)
  }
  if (httr::http_type(resp) != "application/json") {
    stop(
      "API did not return json. Please report this error:\n", 
      "  https://github.com/ropensci/plotly/issues/new",
      call. = FALSE
    )
  }
  # HTTP error message
  msg <- httr::http_status(resp)[["message"]]
  # Custom plotly server error message(s)
  con <- httr::content(resp)
  msgs <- lapply(con$errors, "[[", "message")
  stop(msg, "\n\t", paste(msgs, collapse = "\n\t"), call. = FALSE)
  invisible(resp)
}
