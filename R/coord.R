#' *** This won't be possible until plotly.js implements aspect ratios... ***
#' 
#' #' Force the aspect ratio according to x and y scales
#' #' 
#' #' When x and y are numeric variables measured on the same scale,
#' #' or are related in some meaningful way, forcing the aspect ratio of the
#' #' plot to be proportional to the ratio of a unit change in x versus y improves
#' #' our ability to correctly perceive the data.
#' #' 
#' #' @param p a plotly object
#' #' @param ratio aspect ratio, expressed as y / x
#' #' @export
#' #' @examples 
#' #' 
#' #' canada <- map_data("world", "canada")
#' #' 
#' #' canada %>%
#' #'   group_by(group) %>%
#' #'   plot_ly(x = ~long, y = ~lat, alpha = 0.2) %>%
#' #'   add_polygons(hoverinfo = "none", color = I("black")) %>%
#' #'   coord_fix()
#' #' 
#' #' # works on (non-faceted) ggplot2 plots, too
#' #' gg <- ggplot(canada, aes(long, lat, group = group)) + 
#' #'   geom_polygon() + coord_fixed()
#' #' 
#' #' gg %>%
#' #'   ggplotly() %>%
#' #'   coord_fix()
#' #'   
#' 
#' coord_fix <- function(p, ratio = 1) {
#'   p <- plotly_build(p)
#'   # this won't work for subplots, or categorical data
#'   x <- grepl("^xaxis", names(p$x$layout))
#'   y <- grepl("^yaxis", names(p$x$layout))
#'   if (sum(x) > 1 || sum(y) > 1) {
#'     stop("Can not impose aspect ratio a plot with more than one x/y axis", call. = FALSE)
#'   }
#'   xDat <- unlist(lapply(p$x$data, "[[", "x"))
#'   yDat <- unlist(lapply(p$x$data, "[[", "y"))
#'   if (!is.numeric(xDat) || !is.numeric(yDat)) {
#'     stop("Must have numeric data on both x and y axes to enforce aspect ratios", call. = FALSE)
#'   }
#'   
#'   # warn about any pre-populated domains, they will get squashed
#'   xDom <- p$x$layout[["xaxis"]]$domain %||% c(0, 1)
#'   yDom <- p$x$layout[["yaxis"]]$domain %||% c(0, 1)
#'   if (!identical(yDom, c(0, 1)) || !identical(xDom, c(0, 1))) {
#'     warning(
#'       "coord_fix() won't respect prespecified axis domains (other than the default)",
#'       call. = FALSE
#'     )
#'   }
#'   
#'   xRng <- range(xDat, na.rm = TRUE)
#'   yRng <- range(yDat, na.rm = TRUE)
#'   asp <- ratio * diff(yRng) / diff(xRng)
#'   if (asp < 1) {
#'     p$x$layout[["yaxis"]]$domain <- c(0 + asp / 2, 1 - asp / 2)
#'   } else {
#'     asp <- 1 / asp
#'     p$x$layout[["xaxis"]]$domain <- c(0 + asp / 2, 1 - asp / 2)
#'   }
#'   p
#' }
