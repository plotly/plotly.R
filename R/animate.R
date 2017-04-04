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
#' update graphical elements that can't be transitioned.
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
                           redraw = TRUE, mode = "immediate") {
  
  p$animation <- animation_opts_format(
    frame = frame,
    transition = transition,
    easing = easing,
    redraw = redraw,
    mode = mode
  )
  
  p
}


animation_opts_format <- function(frame, transition, easing, redraw, mode) {
  if (frame < 0) {
    stop("frame must be non-negative.", call. = FALSE)
  }
  if (transition < 0) {
    stop("transition must be non-negative.", call. = FALSE)
  }
  if (frame < transition) {
    stop("frame must be a value larger than transition (it includes the transition)", call. = FALSE)
  }
  
  e <- match.arg(easing, easingOpts())
  m <- match.arg(mode, c('immediate', 'next', 'afterall'))
  
  list(
    transition = list(
      duration = transition,
      easing = easing
    ),
    frame = list(
      duration = frame,
      redraw = redraw
    ),
    mode = mode
  )
}

# a la highlight_defaults()
animation_opts_defaults <- function() {
  opts <- formals(animation_opts)[-1]
  
  # yayyyy for lazy evaluation of arguments
  isQuoted <- identical(opts$transition, quote(frame))
  opts$transition <- if (isQuoted) opts$frame else opts$transition
  
  # flag these as plotly defaults 
  opts <- rapply(opts, default, how = "list")
  
  animation_opts_format(
    frame = opts$frame,
    transition = opts$transition,
    easing = opts$easing,
    redraw = opts$redraw,
    mode = opts$mode
  )
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
animation_button_supply <- function(p) {
  nmenus <- length(p$x$layout$updatemenus)
  isAniButton <- vapply(p$x$layout$updatemenus, is_ani_button, logical(1))
  idx <- if (sum(isAniButton) == 1) which(isAniButton) else nmenus + 1
  p$x$layout$updatemenus[[idx]] <- animation_button_create(p$animation)
  p
}

animation_button_create <- function(opts = animation_opts_defaults()) {
  
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
animation_slider_supply <- function(p, ...) {
  nsliders <- length(p$x$layout$sliders)
  isAniSlider <- vapply(p$x$layout$sliders, is_ani_slider, logical(1))
  hasAniSlider <- sum(isAniSlider) == 1
  idx <- if (hasAniSlider) which(isAniSlider) else nsliders + 1
  p$x$layout$sliders[[idx]] <- animation_slider_create(p, ...)
  p
}

animation_slider_create <- function(p, ...) {
  steps <- lapply(p$x$frames, function(f) {
    # frame names should already be formatted
    nm <- f[["name"]]
    args <- list(list(nm))
    args[[2]] <- p$animation %||% animation_opts_defaults()
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
