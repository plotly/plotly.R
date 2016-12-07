#' Animation configuration options
#' 
#' Animations can be created by either using the \code{frame} argument in 
#' \code{\link{plot_ly}()} or the (unofficial) \code{frame} ggplot2 aesthetic in 
#' \code{\link{ggplotly}()}. By default, animations populate a play button
#' and slider component for controlling the state of the animation
#' (to pause an animation, click on a relevant location on the slider bar). 
#' Both the play button and slider component transition between frames according 
#' rules specified by \code{\link{animation_opts}()}. 
#'
#' @param p a plotly object.
#' @param frame The amount of time between frames (in milliseconds).
#' Note that this amount should include the \code{transition}.
#' @param transition The duration of the smooth transition between
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
#' @rdname animation
#' @author Carson Sievert
#' @examples
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
#' # it's a good idea to remove smooth transitions when there is
#' # no relationship between objects in each view
#' plot_ly(mtcars, x = ~wt, y = ~mpg, frame = ~cyl) %>%
#'   animation_opts(transition = 0)
#'
#' # works the same way with ggplotly
#' if (interactive()) {
#'   p <- ggplot(txhousing, aes(month, median)) +
#'     geom_line(aes(group = year), alpha = 0.3) +
#'     geom_smooth() +
#'     geom_line(aes(frame = year, ids = month), color = "red") +
#'     facet_wrap(~ city)
#'  
#'   ggplotly(p, width = 1200, height = 900) %>%
#'     animation_opts(1000)
#' }
#' 
#'   
#' #' # for more, see https://cpsievert.github.io/plotly_book/key-frame-animations.html
#'
animation_opts <- function(p, frame = 500, transition = frame, easing = "linear",
                           redraw = FALSE, mode = "immediate") {
  if (frame < 0) {
    stop("frame must be non-negative.", call. = FALSE)
  }
  if (transition < 0) {
    stop("frame must be non-negative.", call. = FALSE)
  }
  if (frame < transition) {
    stop("frame must be larger than transition", call. = FALSE)
  }

  opts <- list(
    transition = list(
      duration = transition,
      easing = match.arg(easing, easingOpts())
    ),
    frame = list(
      duration = frame,
      redraw = redraw
    ),
    mode = match.arg(mode, c('immediate', 'next', 'afterall'))
  )

  # build step will ensure we can access the animation frames
  # (required to fill the steps in correctly)
  p <- plotly_build(p)

  # overwrite the animation options in the slider/button spec
  supply_ani_slider(supply_ani_button(p, opts = opts), opts = opts)
}


#' @inheritParams animation_opts
#' @param hide remove the animation slider?
#' @param ... for \code{animation_slider}, attributes are passed to a special
#' layout.sliders object tied to the animation frames. 
#' The definition of these attributes may be found here 
#' \url{https://github.com/plotly/plotly.js/blob/master/src/components/sliders/attributes.js}
#' For \code{animation_button}, arguments are passed to a special 
#' layout.updatemenus button object tied to the animation
#' \url{https://github.com/plotly/plotly.js/blob/master/src/components/updatemenus/attributes.js}
#' @export
#' @rdname animation
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


#' @inheritParams animation_slider
#' @export
#' @rdname animation
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
    showactive = FALSE,
    y = 0,
    x = 0,
    yanchor = 'top',
    xanchor = 'right',
    pad = list(t = 60, r = 5),
    # https://github.com/plotly/plotly.js/issues/1221#issuecomment-264870980
    buttons = list(list(
      label = 'Play',
      method = 'animate',
      args = list(NULL, modify_list(list(fromcurrent = TRUE, mode = "immediate"), opts))
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
  # don't let the user override steps
  slider$steps <- steps

  # set some opinionated defaults
  slider$visible <- slider$visible %||% TRUE
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
