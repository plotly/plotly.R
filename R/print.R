#' Print method for a 'generic' API response
#'
#' @param x a list.
#' @param ... additional arguments (currently ignored)
#' @export
print.api <- function(x, ...) {
  cat("<Response from plot.ly>\n")
  str(x)
  x
}



#' Print a plot on plotly's platform
#'
#' @param x a plotly figure object
#' @param ... additional arguments (currently ignored)
#' @export
print.api_plot <- function(x, ...) {
  utils::browseURL(api_plot_url(x))
  x
}

#' Embed a plotly figure as an iframe in a knitr doc
#'
#' @param x a plotly figure object
#' @param options knitr options.
#' @param ... placeholder.
#' @export
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
knit_print.api_plot <- function(x, options, ...) {
  try_library("knitr")
  iframe <- plotly_iframe(
    api_plot_url(x, embed = TRUE), 
    options[["width"]] %||% "800", 
    options[["height"]] %||% "600"
  )
  knitr::asis_output(iframe)
}

api_plot_url <- function(x, embed = FALSE) {
  url <- if (embed) x$embed_url %||% x$file$embed_url else x$web_url %||% x$file$web_url
  secret <- x$share_key_enabled %||% x$file$share_key_enabled %||% FALSE
  key <- x$share_key %||% x$file$share_key
  if (secret) paste0(url, "?share_key=", key) else url
}

#' Print a plotly grid object
#'
#' @param x a plotly grid object
#' @param ... additional arguments (currently ignored)
#' @export
print.api_grid <- function(x, ...) {
  utils::browseURL(api_grid_url(x))
  x
}

#' Embed a plotly grid as an iframe in a knitr doc
#'
#' @param x a plotly grid object
#' @param options knitr options.
#' @param ... placeholder.
#' @export
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
knit_print.plotly_grid <- function(x, options, ...) {
  try_library("knitr")
  iframe <- plotly_iframe(
    api_grid_url(x, embed = TRUE), 
    options[["width"]] %||% "800", 
    options[["height"]] %||% "600"
  )
  knitr::asis_output(iframe)
}


api_grid_url <- function(x, embed = FALSE) {
  fid <- x$fid %||% x$file$fid
  secret <- x$share_key_enabled %||% x$file$share_key_enabled %||% FALSE
  key <- x$share_key %||% x$file$share_key
  if (embed) {
    paste0(x$embed_url %||% x$file$embed_url, if (secret) paste0("?share_key=", key))
  } else {
    # encourage people to use the create platform
    paste0("https://plot.ly/create/?fid=", fid, if (secret) paste0("&share_key=", key))
  }
}

#' Print a plotly grid object
#'
#' @param x a plotly grid object
#' @param ... additional arguments (currently ignored)
#' @export
print.api_grid_local <- function(x, ...) {
  res <- tryCatch(tibble::as_tibble(x$preview), error = function(e) x$preview)
  print(res)
}
