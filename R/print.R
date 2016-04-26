#' Print a plotly object
#'
#' @param x a plotly object
#' @param ... additional arguments
#' @export
print.plotly_hash <- function(x, ...) {
  x <- as.widget(x)
  print(x, ...)
}

#' Print a 'built' plotly object
#' 
#' @param x a plotly object
#' @param ... additional arguments
#' @export
print.plotly_built <- print.plotly_hash

#' Print a plotly object in a knitr doc
#'
#' @param x a plotly object
#' @param options knitr options.
#' @param ... additional arguments
#' @export
knit_print.plotly_hash <- function(x, options, ...) {
  x <- as.widget(x)
  knitr::knit_print(x, options, ...)
}

#' Print a 'built' plotly object in a knitr doc
#' 
#' @param x a plotly object
#' @param ... additional arguments
#' @export
knit_print.plotly_built <- knit_print.plotly_hash

#' Convert a plotly object to an htmlwidget object
#'
#' @param x a plotly object.
#' @param ... other options passed onto \code{htmlwidgets::createWidget}
#' @export
#' @examples \dontrun{
#' p <- plot_ly(mtcars, x = mpg, y = disp, mode = "markers")
#' htmlwidgets::saveWidget(as.widget(p), "index.html")
#' }
#'

as.widget <- function(x, ...) {
  if (inherits(x, "htmlwidget")) return(x)
  if (!inherits(x, "plotly_built")) x <- plotly_build(x)
  # set some margin defaults if none are provided
  x$layout$margin <- modifyList(
    list(b = 40, l = 60, t = 25, r = 10),
    x$layout$margin %||% list()
  )
  x$config$modeBarButtonsToRemove <- 
    i(x$config$modeBarButtonsToRemove %||% "sendDataToCloud")
  x$base_url <- get_domain()
  # customize the JSON serializer (for htmlwidgets)
  attr(x, 'TOJSON_FUNC') <- to_JSON
  htmlwidgets::createWidget(
    name = "plotly",
    x = x,
    width = x$width,
    height = x$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 5,
      browser.fill = TRUE
    ),
    dependencies = crosstalk::dependencies(),
    ...
  )
}

# for legacy reasons
toWidget <- as.widget

#' Print a plotly figure object
#'
#' @param x a plotly figure object
#' @param ... additional arguments (currently ignored)
#' @export
print.figure <- function(x, ...) {
  utils::browseURL(x$url)
}

#' Embed a plotly figure as an iframe in a knitr doc
#'
#' @param x a plotly figure object
#' @param options knitr options.
#' @param ... placeholder.
#' @export
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
knit_print.figure <- function(x, options, ...) {
  if (!requireNamespace("knitr")) {
    warning("Please install.packages('knitr')")
    return(x)
  }
  w <- if (is.null(options[["width"]])) "800" else options[["width"]]
  h <- if (is.null(options[["height"]])) "600" else options[["height"]]
  iframe <- plotly_iframe(x$url, w, h)
  knitr::asis_output(iframe)
}

#' Embed a plotly figure as an iframe into a IPython Notebook
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
    htmlwidgets::saveWidget(as.widget(l), file = basename(file))
    file
  } else {
    paste0(l$url, ".embed")
  }
  iframe <- plotly_iframe(src, width %||% l$width, height %||% l$height)
  get("display_html", envir = asNamespace("IRdisplay"))(iframe)
}

plotly_iframe <- function(url = "", width = NULL, height = NULL) {
  sprintf(
    '<iframe src="%s" width="%s" height="%s" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>',
    url, width %||% "100%", height %||% "400"
  )
}
