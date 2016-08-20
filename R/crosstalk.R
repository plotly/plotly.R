#' Modify crosstalk selection options
#' 
#' Control the visual appearance of selections deriving from a given
#' selection set.
#' 
#' @param p a plotly visualization.
#' @param on turn on a selection on which event(s)? Likely candidates are
#' 'plotly_hover', 'plotly_click', 'plotly_selected'.
#' @param off turn off a selection on which event(s)? Likely candidates are
#' 'plotly_unhover', 'plotly_doubleclick', 'plotly_deselect'.
#' @param dynamic should UI controls for managing selection aesthetics be 
#' included in the output?
#' @param persistent should selections persist (i.e., accumulate)?
#' @param color character string of color(s) to use for 
#' highlighting selections. See \code{\link{toRGB}()} for valid color
#' specifications. If \code{NULL} (the default), the color of selected marks
#' are not altered (only their opacity).
#' @param opacityDim a number between 0 and 1 used to reduce the
#' opacity of non-selected traces (by multiplying with the existing opacity).
#' @param showInLegend populate an additional legend entry for the selection?
#' @export
#' 

crosstalk <- function(p, on = "plotly_selected", off = "plotly_relayout", 
                      color = NULL, dynamic = FALSE, persistent = FALSE,
                      opacityDim = 0.2, showInLegend = FALSE) {
  if (!is.plotly(p)) {
    stop("Don't know how to modify crosstalk options to objects of class:", class(p))
  }
  if (opacityDim < 0 || 1 < opacityDim) {
    stop("opacityDim must be between 0 and 1", call. = FALSE)
  }
  if (dynamic && length(color) < 2) {
    message("Adding more colors to the selection color palette")
    color <- c(color, RColorBrewer::brewer.pal(4, "Set1"))
  }
  if (!dynamic) {
    if (length(color) > 1) {
      warning(
        "Can only use a single color for selections when dynamic=FALSE",
        call. = FALSE
      )
      color <- color[1] 
    }
  }
  p$x$crosstalk <- modify_list(
    p$x$crosstalk,
    list(
      on = on,
      off = off,
      color = toRGB(color),
      dynamic = dynamic,
      persistent = persistent,
      opacityDim = opacityDim,
      showInLegend = showInLegend
    )
  )
  if (dynamic) {
    w <- colourpicker::colourWidget(
      value = color[1],
      palette = "limited",
      allowedCols = color
    )
    w <- htmlwidgets::onRender(w, "
        function(el, x) {
          var $el = $('#' + el.id);
          crosstalk.var('selectionColour').set($el.colourpicker('value'));
          $el.on('change', function() {
            crosstalk.var('selectionColour').set($el.colourpicker('value'));
          })
        }
    ")
    p <- htmltools::browsable(htmltools::tagList(w, p))
  }
  p
}

crosstalk_defaults <- function() {
  formals(crosstalk)[-1]
}
