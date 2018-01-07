#' Embed a plot as an iframe into a Jupyter Notebook
#' @param x a plotly object
#' @param width attribute of the iframe. If `NULL`, the width in
#' `plot_ly` is used. If that is also `NULL`, '100\%' is the default.
#' @param height attribute of the iframe. If `NULL`, the height in
#' `plot_ly` is used. If that is also `NULL`, '400px' is the default.
#' @param file deprecated.
#' @author Carson Sievert
#' @export
embed_notebook <- function(x, width = NULL, height = NULL, file = NULL) {
  try_library("IRdisplay", "embed_notebook")
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

  ext_html <- TRUE
  if( is.null(file) ){
    warning('No file specified. Embedding in notebook')
    ext_html <- FALSE
    file <- tempfile(fileext = ".html")
    on.exit(unlink(file), add = TRUE)
  }

  res <- htmlwidgets::saveWidget(p, file, selfcontained=FALSE)

  # wrap in an iframe as *nteract* won't do this automatically
  if( ext_html ){
    html <- plotly_iframe(
      file, 
      width %||% p$width %||% "100%",
      height %||% p$height %||% 400,
      ""
    )
  }else{
    html <- plotly_iframe(
      base64enc::dataURI(mime = "text/html;charset=utf-8", file = file),
      width %||% p$width %||% "100%", 
      height %||% p$height %||% 400,
      ""
    )
  }

  display(html)
}


plotly_iframe <- function(url = "", width = NULL, height = NULL, url_ext = ".embed") {
  url <- paste0(url, url_ext)
  sprintf(
    '<iframe src="%s" width="%s" height="%s" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>', 
    url, width %||% "100%", height %||% "400"
  )
}
