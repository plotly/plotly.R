#' Animation options
#' 
#' @param p a plotly object.
#' @param frameDuration The duration each frame is shown (in milliseconds).
#' @param transitionDuration The duration of the transition (in milliseconds).
#' @param easing The type of transition easing. See the list of options here
#' \url{https://github.com/plotly/plotly.js/blob/master/src/plots/animation_attributes.js}
#' @param redraw Trigger a redraw of the plot at completion of the transition?
#' A redraw may significantly impact performance, but may be necessary to 
#' update plot attributes that can't be transitioned.
#' @param mode Describes how a new animate call interacts with currently-running
#' animations. If `immediate`, current animations are interrupted and
#' the new animation is started. If `next`, the current frame is allowed
#' to complete, after which the new animation is started. If `afterall`
#' all existing frames are animated to completion before the new animation
#' is started.
#' @export
#' @author Carson Sievert
#' @examples 
#' 
#' 
#' #' # map a 
#' df <- data.frame(
#'   x = c(1, 2, 2, 1, 1, 2),
#'   y = c(1, 2, 2, 1, 1, 2),
#'   z = c(1, 1, 2, 2, 3, 3)
#' )
#' plot_ly(df) %>% 
#'   add_markers(x = 1.5, y = 1.5) %>%
#'   add_markers(x = ~x, y = ~y, frame = ~z)
#' 
#' # works in subplots
#' subplot(
#'   plot_ly(df, x = ~x, y = ~y, frame = ~z),
#'   plot_ly(df, x = ~x, y = ~y, frame = ~z)
#' )
#' 
#' # set the range in plot_ly()
#' plot_ly(mtcars, x = ~wt, y = ~mpg, frame = ~cyl) %>%
#'   layout(
#'     xaxis = ~list(range = range(wt)), 
#'     yaxis = ~list(range = range(mpg))
#'  )
#'  
#' # works the same way with ggplotly
#' 
#' \dontrun{
#' data(gapminder, package = "gapminder")
#' p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, 
#'   color = continent, frame = year)) +
#'   geom_point() +
#'   scale_x_log10()
#' ggplotly(p)
#' 
#' # data for showing the selected year
#' txt <- with(gapminder, data.frame(
#'   yr = unique(year),
#'   x = median(gdpPercap),
#'   y = max(lifeExp)
#' ))
#' 
#' p2 <- ggplot(gapminder, aes(gdpPercap, lifeExp)) +
#'   geom_point(aes(size = pop), alpha = 0.5) +
#'   # animations can be specified on the layer level
#'   geom_point(aes(size = pop, frame = year), color = "red") +
#'   geom_text(data = txt, aes(label = yr, x = x, y = y, frame = yr)) +
#'   scale_x_log10()
#' ggplotly(p2)
#' 
#' }
#' 
animationOpts <- function(p, frameDuration = 500, transitionDuration = 500, 
                          easing = "cubic-in-out", redraw = FALSE, 
                          mode = "immediate") {
  if (frameDuration < 0) {
    stop("frameDuration must be non-negative.", call. = FALSE)
  }
  if (transitionDuration < 0) {
    stop("frameDuration must be non-negative.", call. = FALSE)
  }
  opts <- list(
    transition = list(
      duration = transitionDuration,
      easing = match.arg(easing, easingOpts())
    ),
    frame = list(
      duration = frameDuration,
      redraw = redraw
    ),
    mode = match.arg(mode, c('immediate', 'next', 'afterall'))
  )
  # TODO: add argument to restrict frames?
  click <- sprintf(
    "Plotly.animate(gd, null, %s);", to_JSON(opts)
  )
  # insanity...modeBarButtonsToAdd is an array of objects....
  nms <- vapply(p$x$config$modeBarButtonsToAdd, function(x) x[["name"]] %||% "", character(1))
  if (idx <- play_button()[["name"]] %in% nms) {
    # overwrite the existing play button
    p$x$config$modeBarButtonsToAdd[[which(idx)]]$click <- click
  } else {
    p$x$config$modeBarButtonsToAdd <- list(play_button(click))
  }
  p
}

easingOpts <- function() {
  c('linear', 'quad', 'cubic', 'sin', 'exp', 'circle', 'elastic', 'back', 
    'bounce', 'linear-in', 'quad-in', 'cubic-in', 'sin-in', 'exp-in', 
    'circle-in', 'elastic-in', 'back-in', 'bounce-in', 'linear-out', 
    'quad-out', 'cubic-out', 'sin-out', 'exp-out', 'circle-out', 'elastic-out', 
    'back-out', 'bounce-out', 'linear-in-out', 'quad-in-out', 'cubic-in-out', 
    'sin-in-out', 'exp-in-out', 'circle-in-out', 'elastic-in-out', 
    'back-in-out', 'bounce-in-out') 
}
