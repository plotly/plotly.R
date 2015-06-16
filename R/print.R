# ----------------------------------------------------------------------------
# Printing methods
# ----------------------------------------------------------------------------

#' Print a plotly object
#' 
#' @param x an object with class 'plotly'
#' @param ... other arguments
#' @export
print.plotly <- function(x, ...) {
  hash <- attr(x, "plotly_hash")
  if (is.null(hash)) 
    stop("A plotly object should always have a plotly_hash attribute.",
         call. = FALSE)
  l <- plotlyEnv[[hash]]
  resp <- plotly_POST(l)
  l$url <- u <- resp$url
  if (!is.null(u)) {
    if (httr::url_ok(u) && interactive()) browseURL(u)
  }
  invisible(l)
}

#' Embed a plotly iframe into an R markdown document via \code{knit_print}
#' @param x named list of ggplots and option lists to pass to \code{animint2dir}.
#' @param options knitr options.
#' @param ... placeholder.
#' @export
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
knit_print.plotly <- function(x, options, ...) {
  if (!requireNamespace("knitr")) {
    warning("Please install.packages('knitr')")
    return(x)
  }
  resp <- print(x)
  w <- if (is.null(options[["width"]])) "800" else options[["width"]]
  h <- if (is.null(options[["height"]])) "600" else options[["height"]]
  iframe <- plotly_iframe(resp$url, w, h)
  knitr::asis_output(iframe)
}

#' Embed a plotly iframe into a IPython Notebook
#' @param x a plotly object
#' @param width attribute of the iframe
#' @param height attribute of the iframe
#' @export
embed_notebook <- function(x, width = "100%", height = "525") {
  if (!requireNamespace("IRdisplay")) {
    warning("You need the IRdisplay package to use this function: \n",
            "devtools::install_github(c('IRkernel/repr', 'IRKernel/IRdisplay'))")
    return(p)
  }
  resp <- print(x)
  iframe <- plotly_iframe(attr(resp, "url"), width, height)
  IRdisplay::display_html(iframe)
}

plotly_iframe <- function(url, width, height) {
  paste("<iframe height=\"", height, "\" id=\"igraph\" scrolling=\"no\" seamless=\"seamless\" src=\"",
        url, "\" width=\"", width, "\" frameBorder=\"0\"></iframe>", sep="")
}
