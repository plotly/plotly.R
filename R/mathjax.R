#' Render TeX in a plotly graph using MathJax
#' 
#' This function makes it slightly easier to render TeX in a plotly graph --
#' it ensures that MathJax is included with the final result and also
#' ensures the provided string is surrounded with `$` (this is what plotly.js
#' uses to declare a string as TeX).
#' 
#' @param x a character vector
#' @export
#' @seealso [config]
#' @examples 
#' 
#' plot_ly(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16)) %>%
#'   layout(title = TeX("\\text{Some mathjax: }\\alpha+\\beta x")) %>%
#'   config(mathjax = "cdn")

TeX <- function(x) {
  startsWithDollar <- grepl("^\\$", x)
  endsWithDollar <- grepl("\\$$", x)
  x <- paste0(if (!startsWithDollar) "$", x, if (!endsWithDollar) "$")
  prefix_class(x, "TeX")
}

is.TeX <- function(x) {
  inherits(x, "TeX")
}

mathjax_cdn <- function() {
  htmltools::htmlDependency(
    name = "mathjax",
    version = "2.7.4",
    src = c(file = depPath("mathjax")),
    script = "cdn.js"
  )
}

# TODO: wait until there is a more official way to include query parameters?
# https://github.com/rstudio/htmltools/issues/98
mathjax_local <- function() {
  path <- mathjax_path()
  
  mj <- file.path(path, "MathJax.js")
  if (!file.exists(mj)) stop("Couldn't locate MathJax.js")
  
  # parse the version
  mathjax <- readLines(mj)
  pat <- 'MathJax.fileversion="[0-9].[0-9].[0-9]'
  ver <- regmatches(mathjax, regexpr(pat, mathjax))
  ver <- sub('"', '', strsplit(ver, "=")[[1]][2])
  
  # make sure we have access to the right config
  config <- file.path(path, "config", "TeX-AMS-MML_SVG.js")
  if (!file.exists(config)) stop("Couldn't locate necessary MathJax config: TeX-AMS-MML_SVG")
  
  htmltools::htmlDependency(
    name = "mathjax", 
    version = ver, 
    src = path,
    script = c("MathJax.js", "config/TeX-AMS-MML_SVG.js")
  )
}


mathjax_path <- function() {
  path <- Sys.getenv("PLOTLY_MATHJAX_PATH", NA)
  
  if (!is.na(path)) {
    mj <- file.path(path, "MathJax.js")
    if (!file.exists(mj)) stop("Couldn't find 'MathJax.js' file in local directory")
    return(path)
  }
  
  stop(
    "To use a local version of MathJax with plotly, set the PLOTLY_MATHJAX_PATH",
    "environment variable to the location of MathJax.",
    call. = FALSE
  )
}
