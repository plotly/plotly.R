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

#' Convert a plotly object to an htmlwidget object
#' 
#' Users shouldn't need to use this function. It's exported for internal reasons.
#' 
#' @param x a plotly object.
#' 
toWidget <- function(x) {
  p <- plotly_build(x)
  # set some margin defaults if none are provided
  p$layout$margin <- modifyList(
    list(b = 40, l = 60, t = 25, r = 10),
    p$layout$margin %||% list()
  )
  # customize the JSON serializer (for htmlwidgets)
  attr(p, 'TOJSON_FUNC') <- to_JSON
  htmlwidgets::createWidget(
    name = "plotly",
    x = p,
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
