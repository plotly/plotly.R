#' Bind an event to a plotly htmlwidget
#' 
#' This functionality is an experiment
#' 
#' @param p a plotly visualization
#' @param event A valid DOM event 
#' @param callback character string containing the body of a JavaScript 
#' callback function with the arguments \code{event} and \code{data} 
#' to be applied to the plotly instance.
#' @export
#' @importFrom htmlwidgets JS
#' @examples 
#' 
#' p <- plot_ly(mtcars, x = mpg, y = disp, mode = "markers")
#' # open this in a web browser, click on a point, and open your console
#' bind(p)
#' # access the clicked point and display alert box
#' cb <- c(
#'   "var pts = data.points[0];",
#'   "alert('You selected ['+pts.x+', '+pts.y+']');"
#'  )
#' bind(p, callback = cb)
#'

bind <- function(p, event = "plotly_click", callback = "console.log(event, data)") {
  p <- last_plot(p)
  if (missing(callback)) callback <- "return null;"
  if (length(callback) == 0) callback <- "return null;"
  p[["callback"]] <- JS(sprintf(
    "$('#%s').bind('%s', function(event, data) { %s })", 
    p$elementId, event, JS(callback)
  ))
  hash_plot(if (is.data.frame(p)) p else list(), p)
}
