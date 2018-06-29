#' Static image export via orca
#' 
#' The function makes a system call to the orca command-line utility, 
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
#' @param mathjax whether or not to include MathJax (required to render [TeX]).
#' If `TRUE`, the PLOTLY_MATHJAX_PATH environment variable must be set and point 
#' to the location of MathJax (this variable is also used to render [TeX] in 
#' interactive graphs, see [config]).
#' @param parallel_limit Sets the limit of parallel tasks run.
#' @param verbose Turn on verbose logging on stdout.
#' @param debug Starts app in debug mode and turn on verbose logs on stdout.
#' @param safe Turns on safe mode: where figures likely to make browser window 
#' hang during image generating are skipped.
#' @export
#' @author Carson Sievert
#' @examples
#' 
#' \dontrun{
#' p <- plot_ly(z = ~volcano) %>% add_surface()
#' orca(p, "surface-plot.svg")
#' }
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
    "graph", to_JSON(b$x[c("data", "layout")]), 
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
  if (isTRUE(mathjax)) args <- c(args, "--mathjax", file.path(mathjax_path(), "MathJax.js"))
     
  # TODO: point to local topojson? Should this only work if plot_geo(standalone = TRUE)?
  try_library("processx", "orca")
  invisible(processx::run("orca", args, echo = TRUE, spinner = TRUE))
}
