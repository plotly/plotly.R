#' Export a plotly graph to a static file
#' 
#' This function is in the process of being deprecated (use [orca] instead).
#' 
#' @details For SVG plots, a screenshot is taken via `webshot::webshot()`.
#' Since `phantomjs` (and hence `webshot`) does not support WebGL,
#' the RSelenium package is used for exporting WebGL plots. 
#' 
#' @param p a plotly or ggplot object.
#' @param file a filename. The file type is inferred from the file extension.
#' Valid extensions include 'jpeg' | 'png' | 'webp' | 'svg' | 'pdf'
#' @param selenium used only when `p` is a WebGL plot or the output 
#' format is 'webp' or 'svg'. Should be an object of class "rsClientServer"
#' returned by `RSelenium::rsDriver`.
#' @param ... if `p` is non-WebGL and the output file format is 
#' jpeg/png/pdf arguments are passed along to `webshot::webshot()`.
#' Otherwise, they are ignored.
#' @export
#' @author Carson Sievert
#'
export <- function(p = last_plot(), file = "plotly.png", selenium = NULL, ...) {
  .Deprecated("orca")
  
  # infer the file type
  fileType <- tolower(tools::file_ext(file))
  if (!fileType %in% c('jpeg', 'png', 'webp', 'svg', 'pdf')) {
    stop("File type ", fileType, " not supported", call. = FALSE)
  }
  if (is.webgl(p) && fileType %in% "pdf") {
    stop(
      "A personal (or professional) plan is required to export WebGL to pdf:\n",
      "https://plot.ly/products/cloud/",
      call. = FALSE
    )
  }
  
  # webshot only support non-webgl jpeg/png/pdf
  use_webshot <- !is.webgl(p) && fileType %in% c('jpeg', 'png', 'pdf')
  
  if (!use_webshot) {
    # download the image when widget is done rendering
    cmd <- sprintf(
      "function(el, x) {
        var gd = document.getElementById(el.id); 
        Plotly.downloadImage(gd, {format: '%s', width: %s, height: %s, filename: '%s'});
      }", 
      fileType, 
      p$width %||% p$layout$width %||% 800, 
      p$height %||% p$layout$height %||% 600, 
      tools::file_path_sans_ext(file)
    )
    p <- htmlwidgets::onRender(p, cmd)
  }
  
  # save widget to an HTML file
  f <- basename(tempfile('plotly', '.', '.html'))
  on.exit(unlink(f), add = TRUE)
  html <- htmlwidgets::saveWidget(p, f)
  
  # phantomjs doesn't support webgl or svg/webp output
  if (use_webshot) {
    try_library("webshot", "export")
    return(webshot::webshot(f, file, ...))
  }
  
  if (inherits(selenium, "rsClientServer")) {
    # TODO: does this work cross-platform?
    selenium$client$navigate(paste0("file://", normalizePath(f)))
  } else {
    stop(
      "Must provide an object of class 'rsClientServer' to the `selenium` ",
      "argument to export this plot (see examples section on `help(export)`)",
      call. = FALSE
    )
  }
  message(
    sprintf("Success! Check your downloads folder for a file named: '%s'", file)
  )
  invisible(file)
}
