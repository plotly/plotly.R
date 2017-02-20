#' Animate a collection of plots
#' 
#' @param ... a collection of ggplot2 or plotly objects
#' @param slider whether to populate a slider to control animation
#' @param button whether to populate a play button (recommended).
#' 
#' @export
#' @author Carson Sievert
#' @examples 
#' 
#' p <- plot_ly(x = LETTERS, y = seq_along(LETTERS), color = LETTERS) %>% hide_legend()
#' p <- plot_ly(x = LETTERS, y = seq_along(LETTERS))# %>% hide_legend()
#' a <- animate(
#'   add_markers(p), add_bars(p)#, 
#'   #plot_ly(labels = LETTERS, values = seq_along(LETTERS)) %>% add_pie()
#' )
#' # TODO: how to optionally suppress the slider?
#' animation_opts(a, 1000, redraw = TRUE)
#' 
#' 

animate <- function(..., slider = FALSE, button = TRUE) {
  
  plots <- lapply(dots2plots(...), plotly_build)
  
  if (length(plots) == 1) {
    return(plots[[1]])
  }
  
  # move data/layout/frames from plots into frames of 1st plot
  
  plots[[1]]$x$frames <- plots[[1]]$x$frames %||% list()
  for (i in 2:length(plots)) {
    p <- plots[[i]]
    newFrames <- list(c(p$x[c("data", "layout")], p$x[["frames"]]))
    plots[[1]]$x$frames <- c(plots[[1]]$x$frames, newFrames)
  }
  #browser()
  p <- plots[[1]]
  
  if (button) {
    p <- supply_ani_button(p)
  }
  
  if (slider) {
    p <- supply_ani_slider(p)
  }
  
  p
  
}
