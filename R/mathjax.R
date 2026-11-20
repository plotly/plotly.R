# A local MathJax, for orca() (plots use plotly.core's mathjax_dependency())
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
