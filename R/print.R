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
