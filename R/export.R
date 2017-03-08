#' Export a plotly graph to a static file
#' 
#' @details For SVG plots, a screenshot is taken via \code{webshot::webshot()}.
#' Since \code{phantomjs} (and hence \code{webshot}) does not support WebGL,
#' the RSelenium package is used for exporting WebGL plots. 
#' 
#' @param p a plotly or ggplot object.
#' @param file a filename. File extension must be included.
#' @param delay time (in seconds) to wait before taking screenshot/writing to disk. 
#' Sometimes a longer delay is needed for all assets to display properly.
#' @param ... if \code{p} is a webgl plot, arguments are passed along 
#' to \code{RSelenium::rsDriver()}; otherwise, they are passed along to
#' \code{webshot::webshot()}
#' @export
#' @examples \dontrun{
#' export(plot_ly(economics, x = ~date, y = ~pce))
#' export(plot_ly(economics, x = ~date, y = ~pce, z = ~pop))
#' }
export <- function(p = last_plot(), file = "plotly.png", delay = 2, ...) {
  # save to an HTML file (and attach id so we may query graphdiv later on)
  f <- basename(tempfile('plotly', '.', '.html'))
  on.exit(unlink(f), add = TRUE)
  p$elementId <- "myPlot"
  html <- htmlwidgets::saveWidget(p, f)
  
  # phantomjs doesn't support webgl :(
  if (!is.webgl(p)) {
    return(webshot::webshot(f, file, delay = delay, ...))
  }
  
  if (system.file(package = "RSelenium") == "") {
    stop("Exporting WebGL requires the RSelenium package:\n", 
         "install.packages('RSelenium')", call. = FALSE)
  }
  continue <- readline(
    "Exporting WebGL may require downloading/installing extraneous software, continue? [y/n]"
  )
  if (!interactive() || grepl("[yY]", continue)) {
    # start up a selenium webdriver and navigate to the HTML file
    rD <- RSelenium::rsDriver(browser = "chrome", verbose = FALSE, ...)
    on.exit(rD$server$stop(), add = TRUE)
    # TODO: does this work cross-platform?
    rD$client$navigate(paste0("file://", normalizePath(f)))
    width <- p$width %||% p$layout$width %||% 800
    height <- p$height %||% p$layout$height %||% 600
    cmd <- sprintf(
      "var gd = document.getElementById('myPlot'); Plotly.downloadImage(gd, {format: '%s', width: %s, height: %s, filename: '%s'});", 
      tools::file_ext(file), width, height, tools::file_path_sans_ext(file)
    )
    rD$client$executeScript(cmd)
    # wait for file to write to disk
    Sys.sleep(delay)
  }
  message(
    sprintf("Success! Check your downloads folder for a file named: '%s'", file)
  )
  invisible(file)
}


#library(htmlwidgets)
#plot_ly(economics, x = ~date, y = ~uempmed) %>%
#  onRender(
#    "function(el, x) {
#       var gd = document.getElementById(el.id);
#      Plotly.downloadImage(gd, {format: 'jpeg', height: 300, width: 300, filename: 'plotly_download'})
#    }"
#  )
