#' Trigger an animation
#' 
#' @param ... One of the following 
#' \itemize{
#'  \item any number of plotly/ggplot2 objects.
#'  \item a list of plotly/ggplot2 objects.
#'  \item a tibble with one list-column of plotly/ggplot2 objects.
#' }
#' @param options animation options. See \code{\link{animationOpts}()}.
#' @export
#' @author Carson Sievert
#' @seealso \code{\link{animationOpts}()}
#' @examples 
#' 
#' cols <- I(c("red", "blue"))
#' animate(
#'   plot_ly(x = c(1, 2), y = c(1, 2), color = cols),
#'   plot_ly(x = c(2, 1), y = c(2, 1), color = cols),
#'   plot_ly(x = c(1, 2), y = c(1, 2), color = cols)
#' ) 
#' 
#' # TODO: prepopulate layout$xaxisid$range so stuff doesn't go running off the page
#' mtcars %>%
#'  split(.$cyl) %>%
#'  lapply(function(d) plot_ly(d, x = ~wt, y = ~mpg)) %>%
#'  animate(options = animationOpts(redraw = T))
#' 

animate <- function(..., options = animationOpts()) {
  
  # build each plot
  plotz <- lapply(dots2plots(...), function(d) plotly_build(d)[["x"]])
  nPlots <- length(plotz)
  
  # animate assumes everything provided to ... lives on the same set of axes
  # first, check to make sure there aren't too many axes
  layoutAttrs <- unique(unlist(lapply(plotz, function(p) { names(p$layout) })))
  if (length(grep("^xaxis", layoutAttrs)) > 1) {
    stop("Can't have more than one xaxis in a single animation", call. = FALSE)
  }
  if (length(grep("^yaxis", layoutAttrs)) > 1) {
    stop("Can't have more than one yaxis in a single animation", call. = FALSE)
  }
  if (length(grep("^geo", layoutAttrs)) > 1) {
    stop("Can't have more than one geo layout in a single animation", call. = FALSE)
  }
  if (length(grep("^mapbox", layoutAttrs)) > 1) {
    stop("Can't have more than one mapbox layout in a single animation", call. = FALSE)
  }
  # anchor references also have to be complete
  anchors <- lapply(plotz, function(p) {
    lapply(p$data, function(tr) {
      compact(tr[c("xaxis", "yaxis", "geo", "subplot")])
    })
  })
  anchorz <- unique(sub("^x$", "xaxis", sub("^y$", "yaxis", unlist(anchors))))
  if (length(setdiff(anchorz, layoutAttrs))) {
    stop("Trace anchors are referencing layout attributes that don't exist", call. = FALSE)
  }
  
  # animations only work on scatter traces
  for (i in seq_len(nPlots)) {
    p <- plotz[[i]]
    types <- vapply(p$data, function(tr) tr["type"][[1]] %||% "scatter", character(1))
    if (any(!grepl("scatter", types))) {
      warning("Animations only work on non-scatter traces", call. = FALSE)
    }
  }
  
  # every plot but the first is considered a frame
  frames <- plotz[-1]
  frames <- lapply(frames, function(p) {
    p <- p[c("data", "layout")]
    p[["name"]] <- new_id()
    p$data <- lapply(p$data, function(tr) { tr$frame <- TRUE; tr })
    p$layout$frame <- TRUE
    p
  })
  
  p <- list(
    data = plotz[[1]][["data"]], 
    layout = plotz[[1]][["layout"]],
    frames = setNames(frames, NULL),
    animationOpts = options,
    source = ensure_one(plotz, "source"),
    config = ensure_one(plotz, "config")
  )
  
  config(
    as_widget(p), 
    modeBarButtonsToAdd = list(play_button(), pause_button())
  )
}

play_button <- function() {
  list(
    name = "Play",
    icon = list(
      width = 500,
      ascent = 500,
      descent = -50,
      path = 'M11,10 L17,10 17,26 11,26 M20,10 L26,10 26,26 20,26'
    ),
    click = htmlwidgets::JS(
      "function(gd) {    
        Plotly.animate(gd, null);
      }"
    )
  )
}

pause_button <- function() {
  list(
    name = "Pause",
    icon = list(
      width = 1500,
      ascent = 850,
      descent = -150,
      path = 'M11,10 L17,10 17,26 11,26 M20,10 L26,10 26,26 20,26'
    ),
    click = htmlwidgets::JS(
      "function(gd) {    
        Plotly.animate(gd, []);
      }"
    )
  )
}


#' Animation options
#' 
#' @param frameDuration The duration each frame is shown (in milliseconds).
#' @param transitionDuration The duration of the transition (in milliseconds).
#' @param easing The type of transition easing. See the list of options here
#' \url{https://github.com/plotly/plotly.js/blob/master/src/plots/animation_attributes.js}
#' @param redraw Trigger a redraw of the plot at completion of the transition?
#' A redraw may significantly impact performance, but may be necessary to 
#' update plot attributes that can't be transitioned.
#' @seealso \code{\link{animate}()}
#' @export
#' @author Carson Sievert
#' 
animationOpts <- function(frameDuration = 500, transitionDuration = 500, 
                          easing = "cubic-in-out", redraw = FALSE) {
  if (frameDuration < 0) {
    stop("frameDuration must be non-negative.", call. = FALSE)
  }
  if (transitionDuration < 0) {
    stop("frameDuration must be non-negative.", call. = FALSE)
  }
  list(
    transition = list(
      duration = transitionDuration,
      easing = match.arg(easing, easingOpts())
    ),
    frame = list(
      duration = frameDuration,
      redraw = redraw
    ),
    # TODO: expose this as an option?
    mode = "afterall"
  )
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



dots2plots <- function(...) {
  dotz <- list(...)
  
  # if ... is a list (or a tibble), list(...) is a (length 1) list 
  # containing a list of plotly objects
  if (length(dotz) == 1 && is.list(dotz[[1]]) && !is.plotly(dotz[[1]])) {
    dotz <- dotz[[1]]
  }
  
  if (tibble::is_tibble(dotz)) {
    # if dots is a tibble, search for one column with a list of plotly objects
    idx <- which(vapply(dotz, function(x) is.plotly(x[[1]]), logical(1)))
    if (length(idx) != 1) {
      stop(
        "If you supply a tibble to subplot(), \n", 
        "it must have _one_ column with a list of plotly objects",
        call. = FALSE
      )
    }
    dotz <- dotz[[idx]]
  }
  
  dotz
}
