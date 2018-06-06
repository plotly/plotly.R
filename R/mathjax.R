mathjax_cdn <- function() {
  htmltools::htmlDependency(
    name = "mathjax",
    version = "2.7.4",
    src = c(file = depPath("mathjax")),
    script = "cdn.js"
  )
}

# TODO: wait until there is a more official way to include query parameters
# https://github.com/rstudio/htmltools/issues/98
mathjax_local <- function() {
  htmltools::htmlDependency(
    name = "mathjax",
    version = "2.7.4",
    src = c(file = mathjax_path()),
    script = "MathJax.js?config=TeX-AMS-MML_SVG"
  )
}


mathjax_path <- function() {
  path <- Sys.getenv("PLOTLY_MATHJAX_PATH", NA)
  if (!is.na(path)) {
    mj <- file.path(path, "MathJax.js")
    if (!file.exists(mj)) stop("Couldn't find 'MathJax.js' file in local directory")
    return(path)
  }
  
  if (system.file(package = "rmarkdown") != "") {
    # Note, this should throw an informative error if the path isn't found
    return(getFromNamespace("pandoc_mathjax_local_path", "rmarkdown")())
  } 
  
  stop(
    "To use a local version of MathJax with plotly, either set the PLOTLY_MATHJAX_PATH",
    "environment variable to the location of MathJax or install rmarkdown.",
    call. = FALSE
  )
}
