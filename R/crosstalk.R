#' Client-side interaction controls
#' 
#' Control the visual appearance of selections deriving from a given
#' selection set.
#' 
#' @param on turn on a selection on which event(s)? Likely candidates are
#' 'plotly_hover', 'plotly_click', 'plotly_selected'.
#' @param off turn off a selection on which event(s)? Likely candidates are
#' 'plotly_unhover', 'plotly_doubleclick', 'plotly_deselect'.
#' @param dynamic should UI controls for managing selection aesthetics be 
#' included in the output?
#' @param persistant should selections persist (i.e., accumulate)?
#' @param color color(s) to use for highlighting selections. 
#' If \code{NULL} (the default), the color of selections are not altered. 
#' If not \code{NULL}
#' a valid color code,  
#' If not \code{dynamic}, this argument should be length 1,
#' If \code{dynamic},
#' this argument accepts a character
#' @param opacityDim a number between 0 and 1 used to reduce the
#' opacity of non-selected traces (by multiplying with the existing opacity).
#' @param showInLegend show a legend entry for additional "selection traces"?
#' @export
#' 

ct_opts <- function(on = "plotly_selected", off = "plotly_deselect", 
                    color = NULL, dynamic = FALSE, persitant = dynamic,
                    opacityDim = 0.2, showInLegend = FALSE) {
  if (opacityDim < 0 || 1 < opacityDim) {
    stop("opacityDim must be between 0 and 1", call. = FALSE)
  }
  if (dynamic && length(color) < 2) {
    message("Adding more colors to the selection color palette")
    color <- c(color, RColorBrewer::brewer.pal(8, "Set2"))
  }
  if (!dynamic) {
   color <- color[1] 
  }
  list(
    on = if (length(on) == 1) list(on) else on,
    off = if (length(off) == 1) list(off) else off,
    color = toRGB(color),
    dynamic = dynamic,
    persitant = persitant,
    opacityDim = opacityDim,
    showInLegend = showInLegend
  )
}
