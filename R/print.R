# ----------------------------------------------------------------------------
# Printing methods
# ----------------------------------------------------------------------------

#' Print a plotly object
#' 
#' @param p a list of class 'plotly'
#' @export
print.plotly <- function(p) {
  #browser()
  if (is.null(p$data)) stop("No traces detected in plot.")
  # POST to plotly API
  resp <- plotly_POST(p)
  u <- attr(resp, "url")
  if (httr::url_ok(u)) 
    message("Success! View your plotly here -> ", u)
  if (interactive()) browseURL(u)
  resp
}

#' Print a figure object
#'
#' @param fig a figure object
#' @export
print.figure <- function(fig) {
  u <- attr(fig, "url")
  id <- sub(".*/([0-9]+)[/]?.*", "\\1", u)
  usr <- verify("username")
  # only POST this figure if it's different from the one that already exists
  if (!identical(fig, get_figure(usr, id))) {
    fig <- plotly_POST(fig)
    message("Successfully modified figure object! View it here -> ", u)
  }
  if (interactive()) browseURL(u)
  fig
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
  iframe <- plotly_iframe(attr(resp, "url"), w, h)
  knitr::asis_output(iframe)
}

#' @export
knit_print.figure <- knit_print.plotly

plotly_iframe <- function(url, width, height) {
  paste("<iframe height=\"", height, "\" id=\"igraph\" scrolling=\"no\" seamless=\"seamless\" src=\"",
        url, "\" width=\"", width, "\" frameBorder=\"0\"></iframe>", sep="")
}

#' Embed a plotly iframe into a IPython Notebook
#' @param url A url pointing to a plotly graph
#' @param width attribute of the iframe
#' @param height attribute of the iframe
#' @export
embed_notebook <- function(url, width = "100%", height = "525") {
  if (!inherits(p, "clientresp")) {
    p <- plotly(p)
    url <- p[["url"]]
  }
  if (!requireNamespace("IRdisplay")) {
    warning("You need the IRdisplay package to use this function: \n",
            "devtools::install_github(c('IRkernel/repr', 'IRKernel/IRdisplay'))")
    return(p)
  }
  IRdisplay::display_html(plotly_iframe(url, height, width))
}
