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
  if (!requireNamespace("knitr")) {
    warning("Please install.packages('knitr')")
    return(x)
  }
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
  if (!requireNamespace("knitr")) {
    warning("Please install.packages('knitr')")
    return(x)
  }
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


#' Embed a plot as an iframe into a Jupyter Notebook
#' @param x a plotly object
#' @param width attribute of the iframe. If \code{NULL}, the width in
#' \code{plot_ly} is used. If that is also \code{NULL}, '100\%' is the default.
#' @param height attribute of the iframe. If \code{NULL}, the height in
#' \code{plot_ly} is used. If that is also \code{NULL}, '400px' is the default.
#' @param file deprecated.
#' @export
embed_notebook <- function(x, width = NULL, height = NULL, file = NULL) {
  if (system.file(package = "IRdisplay") == "") {
    warning("You need the IRdisplay package to use this function: \n",
            "devtools::install_github(c('IRkernel/repr', 'IRKernel/IRdisplay'))")
    return(x)
  }
  if (!is.null(file)) {
    warning("The file argument is no longer used", call. = FALSE)
  }
  UseMethod("embed_notebook")
}

#' @export
embed_notebook.plotly <- function(x, width = NULL, height = NULL, file = NULL) {
  # TODO: get rid of this and provide method for api_figure objects
  display <- getFromNamespace("display_html", asNamespace("IRdisplay"))
  
  if (!is.null(x$x$url)) {
    html <- plotly_iframe(
      x$x$url,
      width %||% x$width %||% "100%", 
      height %||% x$height %||% 400
    )
    return(display(html))
  }
  p <- plotly_build(x)
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  res <- htmlwidgets::saveWidget(p, tmp)
  # wrap in an iframe as *nteract* won't do this automatically
  html <- plotly_iframe(
    base64enc::dataURI(mime = "text/html;charset=utf-8", file = tmp),
    width %||% p$width %||% "100%", 
    height %||% p$height %||% 400,
    ""
  )
  display(html)
}


plotly_iframe <- function(url = "", width = NULL, height = NULL, url_ext = ".embed") {
  url <- paste0(url, url_ext)
  sprintf(
    '<iframe src="%s" width="%s" height="%s" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>', 
    url, width %||% "100%", height %||% "400"
  )
}
