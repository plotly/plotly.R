#' Export a plotly graph to a static file
#' 
#' @details For SVG plots, a screenshot is taken via \code{webshot::webshot()}.
#' Since \code{phantomjs} (and hence \code{webshot}) does not support WebGL,
#' the RSelenium package is used for exporting WebGL plots. 
#' 
#' @param p a plotly or ggplot object.
#' @param file a filename. The file type is inferred from the file extension.
#' Valid extensions include 'jpeg' | 'png' | 'webp' | 'svg' | 'pdf'
#' @param selenium used only when \code{p} is a WebGL plot or the output 
#' format is 'webp' or 'svg'. Should be an object of class "rsClientServer"
#' returned by \code{RSelenium::rsDriver} (see examples).
#' @param ... if \code{p} is non-WebGL and the output file format is 
#' jpeg/png/pdf arguments are passed along to \code{webshot::webshot()}.
#' Otherwise, they are ignored.
#' @export
#' @author Carson Sievert
#' @examples 
#' # The webshot package handles non-WebGL conversion to jpeg/png/pdf
#' export(plot_ly(economics, x = ~date, y = ~pce))
#' export(plot_ly(economics, x = ~date, y = ~pce), "plot.pdf")
#' 
#' \dontrun{
#'   # svg/webp output or WebGL conversion can be done via RSelenium
#'   if (requireNamespace("RSelenium")) {
#'    rD <- RSelenium::rsDriver(browser = "chrome")
#'    export(
#'      plot_ly(economics, x = ~date, y = ~pce), "plot.svg", rD
#'    )
#'    export(
#'      plot_ly(economics, x = ~date, y = ~pce, z = ~pop), "yay.svg", rD
#'    )
#'   }
#' }
#' 
#' # If you can't get a selenium server running, another option is to
#' # use Plotly.downloadImage() via htmlwidgets::onRender()...
#' # Downloading images won't work inside RStudio, but you can set the viewer
#' # option to NULL to prompt your default web browser
#' options(viewer = NULL)
#' plot_ly(economics, x = ~date, y = ~pce, z = ~pop) %>%
#'   htmlwidgets::onRender(
#'    "function(el, x) {
#'      var gd = document.getElementById(el.id); 
#'      Plotly.downloadImage(gd, {format: 'png', width: 600, height: 400, filename: 'plot'});
#'    }"
#'  )
#'  
export <- function(p = last_plot(), file = "plotly.png", selenium = NULL, ...) {
  # infer the file type
  fileType <- tolower(tools::file_ext(file))
  if (!fileType %in% c('jpeg', 'png', 'webp', 'svg', 'pdf')) {
    stop("File type ", filetype, " not supported", call. = FALSE)
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
    return(webshot::webshot(f, file, delay = delay, ...))
  }
  
  if (inherits(selenium, "rsClientServer")) {
    # TODO: does this work cross-platform?
    selenium$client$navigate(paste0("file://", normalizePath(f)))
  } else {
    stop("`selenium` must be an object of class 'rsClientServer'", call. = FALSE)
  }
  message(
    sprintf("Success! Check your downloads folder for a file named: '%s'", file)
  )
  invisible(file)
}
