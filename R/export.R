#' Export a plotly graph to a static file
#' 
#' @param p a plotly or ggplot object.
#' @param file a filename. See the file argument of \code{webshot::webshot} 
#' for valid extensions.
#' @param ... arguments passed onto \code{webshot::webshot}
#' @export
#' @examples \dontrun{
#' export(plot_ly(economics, x = ~date, y = ~pce))
#' }
export <- function(p, file = "plotly.png", ...) {
  if (system.file(package = "webshot") == "") {
    stop(
      'Please install the webshot package ',
      '(if not on CRAN, try devtools::install_github("wch/webshot"))'
    )
  }
  f <- basename(tempfile('plotly', '.', '.html'))
  on.exit(unlink(f), add = TRUE)
  html <- htmlwidgets::saveWidget(plotly_build(p), f)
  webshot::webshot(f, file, ...)
}


#library(htmlwidgets)
#plot_ly(economics, x = ~date, y = ~uempmed) %>%
#  onRender(
#    "function(el, x) {
#       var gd = document.getElementById(el.id);
#      Plotly.downloadImage(gd, {format: 'jpeg', height: 300, width: 300, filename: 'plotly_download'})
#    }"
#  )
