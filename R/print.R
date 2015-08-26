# ----------------------------------------------------------------------------
# Printing methods
# ----------------------------------------------------------------------------

#' Print a plotly object
#' 
#' @param x an object with class 'plotly'
#' @param ... other arguments
#' @export
print.plotly <- function(x, ...) {
  l <- plotly_POST(x)
  if (!is.null(l$url)) {
    if (interactive()) browseURL(l$url)
  }
  # get_figure() instead?
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

#' Print an "offline" (local) plotly object
#' 
#' @param x an object with class 'offline'
#' @param ... other arguments
#' @export
print.offline <- function(x, ...) {
  off <- offline_bundle(jq = TRUE)
  plotlyjs <- readChar(off, file.info(off)$size, useBytes=TRUE)
  html <- sprintf(
    '<!DOCTYPE html><html lang="en">
     <head>
     	<meta charset="utf-8">
      <script type="text/javascript">%s</script>
     	<title>My Plotly</title>
     </head>
     
     <body>
       %s
     </body>', plotlyjs, with(x, new_offline(data, layout, height, width, id))
  )
  d <- if (is.null(x$out_dir)) {
    tempdir()
  } else {
    if (!dir.exists(x$out_dir)) dir.create(x$out_dir, recursive = TRUE)
    x$out_dir
  }
  index <- file.path(d, "index.html")
  res <- writeLines(html, index)
  if (is.function(x$viewer)) x$viewer(index)
}

#' Embed a plotly iframe into an R markdown document via \code{knit_print}
#' @param x named list of ggplots and option lists to pass to \code{animint2dir}.
#' @param options knitr options.
#' @param ... placeholder.
#' @export
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
knit_print.offline <- function(x, options, ...) {
  if (!requireNamespace("knitr")) {
    warning("Please install.packages('knitr')")
    return(x)
  }
  p <- with(x, new_offline(data, layout, height, width, id))
  # if this is the first plot, place bundle just before the plot
  if (length(knitr::knit_meta(class = "plotly", clean = FALSE)) == 0) {
    off <- offline_bundle(jq = TRUE)
    b <- readChar(off, file.info(off)$size, useBytes=TRUE)
    p <- paste0(
      sprintf('<script type="text/javascript">%s</script>', b),
      p
    )
  }
  knitr::asis_output(p, meta = list(plotly = structure("", class = "plotly")))
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
    return(x)
  }
  resp <- print(x)
  iframe <- plotly_iframe(attr(resp, "url"), width, height)
  IRdisplay::display_html(iframe)
}

plotly_iframe <- function(url, width, height) {
  paste("<iframe height=\"", height, "\" id=\"igraph\" scrolling=\"no\" seamless=\"seamless\" src=\"",
        url, ".embed\" width=\"", width, "\" frameBorder=\"0\"></iframe>", sep="")
}
