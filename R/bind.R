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
#' # correlation matrix
#' m <- round(cor(mtcars), 3)
#' p <- plot_ly(x = colnames(m), y = row.names(m), z = m, type = "heatmap")
#' # change x/y variables upon clicking on a cell
#' cb1 <- c(
#'   "var pts = data.points[0]",
#'   "crosstalk.var('x').set(pts.x)",
#'   "crosstalk.var('y').set(pts.y)"
#' )
#' p <- bind(p, callback = cb1)
#' 
#' # hack to attach data to an empty plot
#' l <- plotly_build(plot_ly())
#' l$data[[1]] <- c(l$data[[1]], mtcars)
#' cb2 <- c(
#'   "crosstalk.var('x').on('change',  function(e) { Plotly.plot('%s') })",
#' )
#' 
#' bind(l, callback = )
#' 
#' 
#' dplyr::count(mtcars, cyl) %>%
#'   plot_ly(x = cyl, y = n, type = "bar") %>%
#'   bind(callback = "console.log(data.points[0].x)")
#' 
#' dplyr::count(mtcars, cyl) %>%
#'   plot_ly(x = cyl, y = n, type = "bar") %>%
#'   bind(callback = "crosstalk.var('foo').set(data.points[0].x)")
#' 
#' p1 <- plot_ly(dplyr::count(mtcars, cyl), x = cyl, y = n, type = "bar", source = "x")
#' p2 <- plot_ly(mtcars, x = mpg, type = "histogram", target = "x")
#' 
#' library(htmltools)
#' browsable(tagList(as.widget(p1), as.widget(p2)))
#' 
#' w1 <- dplyr::count(mtcars, cyl) %>%
#'   plot_ly(x = cyl, y = n, type = "bar") %>%
#'   bind() %>%
#'   as.widget()
#'   
#'  w2 <- mtcars %>%
#'    plot_ly(x = mpg, type = "histogram") %>%
#'    as.widget()
#' 
#' library(htmltools)
#' browsable(tagList(w1, w2))

bind <- function(p, event = c("plotly_click", "plotly_hover"), 
                 callback = "console.log(event, data)") {
  p <- last_plot(p)
  if (length(callback) == 0) callback <- "return null;"
  p[["callback"]] <- JS(sprintf(
    "$('#%s').bind('%s', function(event, data) { %s })", 
    p$elementId, match.arg(event), JS(callback)
  ))
  hash_plot(if (is.data.frame(p)) p else list(), p)
}
