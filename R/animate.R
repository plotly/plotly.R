#' Animation options
#' 
#' @param p a plotly object.
#' @param frameDuration The amount of time between frames (in milliseconds).
#' Note that this amount should include the \code{transitionDuration}.
#' @param transitionDuration The duration of the smooth transition between
#' frames (in milliseconds).
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
#' @seealso \code{\link{animation_slider}()}, \code{\link{animation_button}()}
#' @examples 
#' 
#' 
#' df <- data.frame(
#'   x = c(1, 2, 2, 1, 1, 2),
#'   y = c(1, 2, 2, 1, 1, 2),
#'   z = c(1, 1, 2, 2, 3, 3)
#' )
#' plot_ly(df) %>% 
#'   add_markers(x = 1.5, y = 1.5) %>%
#'   add_markers(x = ~x, y = ~y, frame = ~z)
#' 
#' 
#' # it's a good idea to remove smooth transitions when there is 
#' # no relationship between objects in each view
#' plot_ly(mtcars, x = ~wt, y = ~mpg, frame = ~cyl) %>%
#'   animation_opts(transitionDuration = 0)
#'  
#' # works the same way with ggplotly
#' p <- ggplot(txhousing, aes(month, median)) + 
#'   geom_smooth() +
#'   geom_line(aes(frame = year, ids = month)) + 
#'   facet_wrap(~ city)
#'  
#' ggplotly(p, width = 1000, height = 500) %>% 
#'   animation_slider(hide = TRUE)
#' 
#' # use the ids attribute to ensure object constancy
#' if (require("gapminder")) {
#'   p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, 
#'     color = continent, frame = year, ids = country)) +
#'     geom_point() +
#'     scale_x_log10()
#'   ggplotly(p)
#'   
#'   p2 <- ggplot(gapminder, aes(gdpPercap, lifeExp)) +
#'     geom_point(aes(size = pop), alpha = 0.5) +
#'     # animations can be specified on the layer level
#'     geom_point(aes(size = pop, frame = year, ids = country), color = "red") +
#'     scale_x_log10()
#'   ggplotly(p2)
#' }
#' 
animation_opts <- function(p, frameDuration = 500, transitionDuration = frameDuration, 
                          easing = "cubic-in-out", redraw = FALSE, mode = "immediate") {
  if (frameDuration < 0) {
    stop("frameDuration must be non-negative.", call. = FALSE)
  }
  if (transitionDuration < 0) {
    stop("frameDuration must be non-negative.", call. = FALSE)
  }
  if (frameDuration < transitionDuration) {
    stop("frameDuration must be larger than transitionDuration", call. = FALSE)
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
  
  # build step will ensure we can access the animation frames
  # (required to fill the steps in correctly)
  p <- plotly_build(p)
  
  # overwrite the animation options in the slider/button spec
  supply_ani_slider(supply_ani_button(p, opts))
}

#' Hide or customize the animation button
#' 
#' @param p a plotly object
#' @param hide remove the animation slider?
#' @param ... attributes passed to the sliders object which controls the animation
#' slider \url{https://github.com/plotly/plotly.js/blob/master/src/components/sliders/attributes.js}
#' @export
#' @author Carson Sievert
#' @seealso \code{\link{animation_opts}()}, \code{\link{animation_button}()}
#' 

animation_slider <- function(p, hide = FALSE, ...) {
  
  p <- plotly_build(p)
  isAniSlider <- vapply(p$x$layout$sliders, is_ani_slider, logical(1))
  if (hide) {
    p$x$layout$sliders[isAniSlider] <- NULL
    return(p)
  }
  p$x$layout$sliders[[which(isAniSlider)]] <- modify_list(
    p$x$layout$sliders[[which(isAniSlider)]], list(...)
  )
  p
  
}

#' Hide or customize the animation button
#' 
#' @param p a plotly object
#' @param ... arguments passed to the updatemenus which controls the play/pause
#' button \url{https://github.com/plotly/plotly.js/blob/master/src/components/updatemenus/attributes.js}
#' @export
#' @author Carson Sievert
#' @seealso \code{\link{animation_opts}()}, \code{\link{animation_button}()}
#' 

animation_button <- function(p, ...) {
  
  p <- plotly_build(p)
  isAniButton <- vapply(p$x$layout$updatemenus, is_ani_button, logical(1))
  p$x$layout$updatemenus[[which(isAniButton)]] <- modify_list(
    p$x$layout$updatemenus[[which(isAniButton)]], list(...)
  )
  p
}


# supply an animation button if it doesn't exist, 
# and _replace_ an existing animation button
supply_ani_button <- function(p, opts = NULL) {
  nmenus <- length(p$x$layout$updatemenus)
  isAniButton <- vapply(p$x$layout$updatemenus, is_ani_button, logical(1))
  idx <- if (sum(isAniButton) == 1) which(isAniButton) else nmenus + 1
  p$x$layout$updatemenus[[idx]] <- create_ani_button(opts)
  p
}

create_ani_button <- function(opts) {
  button <- list(
    type = 'buttons',
    direction = 'right',
    y = 1,
    x = 0,
    yanchor = 'bottom',
    xanchor = 'left',
    pad = list(b = 20, l = 10),
    buttons = list(list(
      label = 'Play',
      method = 'animate',
      args = list(list(), modify_list(list(fromcurrent = TRUE, mode = "immediate"), opts))
    ), list(
      label = 'Pause',
      method = 'animate',
      args = list(list(), list(mode = "immediate"))
    ))
  )
  structure(button, class = "aniButton")
}

is_ani_button <- function(obj) {
  class(obj) %in% "aniButton"
}

# supply an animation slider if it doesn't exist, 
# and _replace_ an existing animation slider
supply_ani_slider <- function(p, opts = NULL, ...) {
  nsliders <- length(p$x$layout$sliders)
  isAniSlider <- vapply(p$x$layout$sliders, is_ani_slider, logical(1))
  hasAniSlider <- sum(isAniSlider) == 1
  idx <- if (hasAniSlider) which(isAniSlider) else nsliders + 1
  p$x$layout$sliders[[idx]] <- create_ani_slider(p, opts, ...)
  p
}


create_ani_slider <- function(p, opts = NULL, ...) {
  steps <- lapply(p$x$frames, function(f) {
    # frame names should already be formatted
    nm <- f[["name"]]
    args <- list(list(nm))
    args[[2]] <- opts
    list(method = "animate", args = args, label = nm, value = nm)
  })
  
  # inherit defaults from any existing slider
  slider <- modify_list(
    p$x$layout$sliders[[vapply(p$x$layout$sliders, is_ani_slider, logical(1))]], list(...)
  )
  slider$visible <- TRUE
  slider$steps <- slider[["steps"]] %||% steps
  slider$pad$t <- slider$pad[["t"]] %||% 40
  structure(slider, class = "aniSlider")
}

is_ani_slider <- function(obj) {
  class(obj) %in% "aniSlider"
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
