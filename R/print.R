#' Print a plotly figure object
#'
#' @param x a plotly figure object
#' @param ... additional arguments (currently ignored)
#' @export
print.plotly_figure <- function(x, ...) {
  utils::browseURL(x$url)
}

#' Embed a plotly figure as an iframe in a knitr doc
#'
#' @param x a plotly figure object
#' @param options knitr options.
#' @param ... placeholder.
#' @export
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
knit_print.plotly_figure <- function(x, options, ...) {
  if (!requireNamespace("knitr")) {
    warning("Please install.packages('knitr')")
    return(x)
  }
  w <- if (is.null(options[["width"]])) "800" else options[["width"]]
  h <- if (is.null(options[["height"]])) "600" else options[["height"]]
  iframe <- plotly_iframe(x$url, w, h)
  knitr::asis_output(iframe)
}

#' Embed a plotly figure as an iframe into a Jupyter Notebook
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
  if (!is.null(x$x$url)) {
    html <- plotly_iframe(
      x$x$url,
      width %||% x$width %||% "100%", 
      height %||% x$height %||% 400
    )
    return(IRdisplay::display_html(html))
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
  IRdisplay::display_html(html)
}


plotly_iframe <- function(url = "", width = NULL, height = NULL, url_ext = ".embed") {
  url <- paste0(url, url_ext)
  sprintf(
    '<iframe src="%s" width="%s" height="%s" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>', 
    url, width %||% "100%", height %||% "400"
  )
}
