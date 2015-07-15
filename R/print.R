# ----------------------------------------------------------------------------
# Printing methods
# ----------------------------------------------------------------------------

#' Print a plotly object
#' 
#' @param x an object with class 'plotly'
#' @param ... other arguments
#' @export
print.plotly <- function(x, ...) {
  resp <- plotly_POST(x)
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

#' Print an "offline" (local) plotly object
#' 
#' @param x an object with class 'offline'
#' @param ... other arguments
#' @export
print.offline <- function(x, ...) {
  off <- get_offline(jq = TRUE)
  plotlyjs <- readChar(off, file.info(off)$size)
  html <- with(x, sprintf(
    '<!DOCTYPE html><html lang="en">
     <head>
     	<meta charset="utf-8">
      <script type="text/javascript">%s</script>
     	<title>My Plotly</title>
     </head>
     
     <body>
       <div class="%s loading" style="color: rgb(50,50,50);">Drawing...</div>
       <div id="%s" style="height: %s; width: %s;" ></div>
       <script type="text/javascript">
         Plotly.plot("%s", %s, %s).then(function() {
           $(".%s.loading").remove();
         })
      </script>
     </body>', 
    plotlyjs, id, id, height, width, id, data, layout, id
  ))
  d <- if (is.null(x$out_dir)) {
    tempdir()
  } else {
    if (!dir.exists(x$out_dir)) dir.create(x$out_dir, recursive = TRUE)
    x$out_dir
  }
  index <- file.path(d, "index.html")
  res <- writeLines(html, index)
  if (!is.null(x$viewer)) x$viewer(index)
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
  # if this is the first plot in the document, 
  # place dependencies just before it
  if (length(knitr::knit_meta(class = "plotly_offline", clean = FALSE)) == 0) {
    b <- readChar(x$bundle, file.info(x$bundle)$size)
    x$html <- paste0(
      sprintf('<script type="text/javascript">%s</script>', b),
      x$html
    )
  }
  knitr::asis_output(x$html, meta = structure("", class = "plotly_offline"))
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
        url, "\" width=\"", width, "\" frameBorder=\"0\"></iframe>", sep="")
}
