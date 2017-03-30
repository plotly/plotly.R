#' Export a plotly graph to a static file
#' 
#' @details For SVG plots, a screenshot is taken via \code{webshot::webshot()}.
#' Since \code{phantomjs} (and hence \code{webshot}) does not support WebGL,
#' the RSelenium package is used for exporting WebGL plots. 
#' 
#' @param p a plotly or ggplot object.
#' @param file a filename. The file type is inferred from the file extension.
#' Valid extensions include 'jpeg' | 'png' | 'webp' | 'svg' | 'pdf'
#' @param delay time (in seconds) to wait before taking screenshot/writing to disk. 
#' Sometimes a longer delay is needed for all assets to display properly.
#' @param ... if \code{p} is a webgl plot, arguments are passed along 
#' to \code{RSelenium::rsDriver()}; otherwise, they are passed along to
#' \code{webshot::webshot()}
#' @export
#' @examples \dontrun{
#' export(plot_ly(economics, x = ~date, y = ~pce))
#' export(plot_ly(economics, x = ~date, y = ~pce), "plot.svg")
#' export(plot_ly(economics, x = ~date, y = ~pce), "plot.pdf")
#' export(plot_ly(economics, x = ~date, y = ~pce, z = ~pop))
#' }
export <- function(p = last_plot(), file = "plotly.png", delay = 2, ...) {
  # infer the file type
  fileType <- tolower(tools::file_ext(file))
  if (!fileType %in% c('jpeg', 'png', 'webp', 'svg', 'pdf')) {
    stop("File type ", filetype, "not supported", call. = FALSE)
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
  
  if (system.file(package = "RSelenium") == "") {
    stop("Exporting WebGL requires the RSelenium package:\n", 
         "install.packages('RSelenium')", call. = FALSE)
  }
  continue <- readline(
    "Exporting requires downloading/installing a selenium webdriver, continue? [y/n]"
  )
  if (!interactive() || grepl("[yY]", continue)) {
    # Start up a selenium webdriver and navigate to the HTML file
    # I'm pretty sure this will clean itself up after the R session ends
    rD <- RSelenium::rsDriver(browser = "chrome", verbose = FALSE, ...)
    # TODO: does this work cross-platform?
    rD$client$navigate(paste0("file://", normalizePath(f)))
  }
  message(
    sprintf("Success! Check your downloads folder for a file named: '%s'", file)
  )
  invisible(file)
}
