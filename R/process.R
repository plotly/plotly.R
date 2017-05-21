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
  type <- resp[["headers"]][["content-type"]]
  # httr (should) know to call png::readPNG() which returns raster array
  tryCatch(
    httr::content(resp, as = "parsed", type = type), 
    error = function(e) httr::content(resp, as = "raw", type = type)
  )
}

# the default for httr::content() doesn't simplify vectors apparently...
json_content <- function(resp) {
  if (length(resp$content) == 0) return(list())
  from_JSON(
    httr::content(
      resp, 
      as = "text", 
      type = resp[["headers"]][["content-type"]],
      encoding = "UTF-8"
    )
  )
}

relay_error <- function(resp) {
  if (!httr::http_error(resp)) {
    return(resp)
  }
  con <- httr::content(resp)
  # if we can't relay the plotly server error messages, return the response
  if (!"errors" %in% names(con)) {
    return(resp)
  }
  msgs <- lapply(con$errors, "[[", "message")
  stop(
    httr::http_status(resp)[["message"]], "\n\t", 
    paste(msgs, collapse = "\n\t"), 
    call. = FALSE
  )
}
