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
#' @param file a filename for saving the standalone HTML
#' (only used if x is a non-figure object)
#' @export
embed_notebook <- function(x, width = NULL, height = NULL,
                           file = paste0("plotlyJupyterHTML/", digest::digest(Sys.time()), ".html")) {
  if (system.file(package = "IRdisplay") == "") {
    warning("You need the IRdisplay package to use this function: \n",
            "devtools::install_github(c('IRkernel/repr', 'IRKernel/IRdisplay'))")
    return(x)
  }
  l <- plotly_build(x)
  src <- if (is.null(l$url)) {
    dir <- dirname(file)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    owd <- setwd(dir)
    on.exit(setwd(owd), add = TRUE)
    htmlwidgets::saveWidget(l, file = basename(file))
    file
  } else {
    l$url
  }
  iframe <- plotly_iframe(src, width %||% l$width, height %||% l$height, url_ext = "")
  get("display_html", envir = asNamespace("IRdisplay"))(iframe)
}

plotly_iframe <- function(url = "", width = NULL, height = NULL, url_ext = ".embed") {
  url <- paste0(url, url_ext)
  sprintf(
    '<iframe src="%s" width="%s" height="%s" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>', 
    url, width %||% "100%", height %||% "400"
  )
}
