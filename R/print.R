#' Print a plotly object
#' 
#' @param x a plotly object
#' @param ... additional arguments (currently ignored)
#' @export
#' @importFrom htmlwidgets createWidget
#' @importFrom htmlwidgets sizingPolicy
print.plotly <- function(x, ...) {
  w <- toWidget(x)
  get("print.htmlwidget", envir = asNamespace("htmlwidgets"))(w)
}

#' Print a plotly object in a knitr doc
#' 
#' @param x a plotly object
#' @param ... additional arguments (currently ignored)
#' @export
knit_print.plotly <- function(x, ...) {
  w <- toWidget(x)
  get("knit_print.htmlwidget", envir = asNamespace("htmlwidgets"))(w)
}

# convert a plotly object to an htmlwidget object
#' @export
toWidget <- function(x) {
  htmlwidgets::createWidget(
    name = "plotly",
    x = plotly_build(x),
    width = x$width,
    height = x$height,
    htmlwidgets::sizingPolicy(
      padding = 5, 
      browser.fill = TRUE
    )
  )
}

#' Embed a plotly iframe into a IPython Notebook
#' @param x a plotly object
#' @param width attribute of the iframe
#' @param height attribute of the iframe
#' @export
embed_notebook <- function(x, width = "100%", height = "525") {
  if (system.file(package = "IRdisplay") == "") {
    warning("You need the IRdisplay package to use this function: \n",
            "devtools::install_github(c('IRkernel/repr', 'IRKernel/IRdisplay'))")
    return(x)
  }
  resp <- plotly_POST(x)
  iframe <- plotly_iframe(attr(resp, "url"), width, height)
  get("display_html", envir = asNamespace("IRdisplay"))(iframe)
}

plotly_iframe <- function(url, width, height) {
  paste("<iframe height=\"", height, "\" id=\"igraph\" scrolling=\"no\" seamless=\"seamless\" src=\"",
        url, ".embed\" width=\"", width, "\" frameBorder=\"0\"></iframe>", sep="")
}
