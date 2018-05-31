#' Static image export via orca
#' 
#' The function requires the orca command-line utility, 
#' see the installation instructions [here](https://github.com/plotly/orca#installation)
#' 
#' @param p a plotly object.
#' @param file output filename.
#' @param format the output format (png, jpeg, webp, svg, pdf, eps).
#' @param scale Sets the image scale. Applies to all output images.
#' @param width Sets the image width. If not set, defaults to `layout.width` value. 
#' Applies to all output images.
#' @param height Sets the image height. If not set, defaults to `layout.height` value. 
#' Applies to all output images.
#' @param mathjax whether or not to specify a path to mathjax (required to export LaTeX characters).
#' This should 'just work' in RStudio, but outside RStudio, you may have to set 
#' the PLOTLY_MATHJAX_PATH environment variable to the location of MathJax.
#' @param parallel_limit Sets the limit of parallel tasks run.
#' @param verbose Turn on verbose logging on stdout.
#' @param debug Starts app in debug mode and turn on verbose logs on stdout.
#' @param safe Turns on safe mode: where figures likely to make browser window 
#' hang during image generating are skipped.
#' @export
#' @author Carson Sievert
#' @examples
#' 
#' p <- plot_ly(z = ~volcano) %>% add_surface()
#' orca(p, "surface-plot.png")
#' orca(p, "surface-plot.svg")
#' orca(p, "surface-plot.pdf")
#' 

orca <- function(p, file = "plot.png", format = tools::file_ext(file), 
                 scale = NULL, width = NULL, height = NULL, mathjax = FALSE,
                 parallel_limit = NULL, verbose = FALSE, debug = FALSE, 
                 safe = FALSE) {
  
  if (Sys.which("orca") == "") {
    stop(
      "The orca command-line utility is required to use the `orca()` function.\n\n",
      "Follow the installation instructions here -- https://github.com/plotly/orca#installation",
      call. = FALSE
    )
  }
  
  b <- plotly_build(p)
  
  # find the relevant plotly.js bundle
  plotlyjs <- plotlyjsBundle(b)
  plotlyjs_file <- file.path(plotlyjs$src$file, plotlyjs$script)
  
  args <- c(
    "graph", plotly:::to_JSON(b$x[c("data", "layout")]), 
    "-o", file,
    "--format", format,
    "--plotlyjs", plotlyjs_file,
    if (debug) "--debug",
    if (verbose) "--verbose",
    if (safe) "--safe-mode"
  )
  
  if (!is.null(scale)) args <- c(args, "--scale", scale)
  if (!is.null(width)) args <- c(args, "--width", width)
  if (!is.null(height)) args <- c(args, "--height", height)
  if (!is.null(parallel_limit)) args <- c(args, "--parallel-limit", parallel_limit)
  if (!is.na(mapbox_token())) args <- c(args, "--mapbox-access-token", mapbox_token())
  if (isTRUE(mathjax)) args <- c(args, "--mathjax", mathjax_path())
     
  # TODO: point to local topojson? Should this only work if plot_geo(standalone = TRUE)?
  try_library("processx", "orca")
  invisible(processx::run("orca", args, echo = TRUE, spinner = TRUE))
}


mathjax_path <- function() {
  if (is_rstudio()) {
    try_library("rmarkdown", "orca")
    return(getFromNamespace("pandoc_mathjax_local_path", "rmarkdown")())
  }
  path <- Sys.getenv("PLOTLY_MATHJAX_PATH", Sys.getenv("RMARKDOWN_MATHJAX_PATH", NA))
  if (!is.na(path)) return(normalizePath(path, mustWork = TRUE))
  stop(
    "Please set either the RMARKDOWN_MATHJAX_PATH or PLOTLY_MATHJAX_PATH ",
    "environment variable to the location of MathJax. ",
    "On Linux systems you can also install MathJax using your system package manager.",
    call. = FALSE
  )
}
