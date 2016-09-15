#' Highlight graphical elements 
#' 
#' Controls the visual appearance of selections deriving from a given
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
#' @param defaultValues a vector of values for setting a "default selection".
#' These values should match the key attribute.
#' @param color character string of color(s) to use for 
#' highlighting selections. See \code{\link{toRGB}()} for valid color
#' specifications. If \code{NULL} (the default), the color of selected marks
#' are not altered (only their opacity).
#' @param opacityDim a number between 0 and 1 used to reduce the
#' opacity of non-selected traces (by multiplying with the existing opacity).
#' @param showInLegend populate an additional legend entry for the selection?
#' @export
#' @examples
#' 
#' d <- crosstalk::SharedData$new(txhousing, ~city)
#' p <- ggplot(d, aes(date, median, group = city)) + geom_line()
#' ggplotly(p, tooltip = "city") %>%
#'   highlight(on = "plotly_hover", color = "red")
#' 

highlight <- function(p, on = "plotly_selected", off = "plotly_relayout", 
                      dynamic = FALSE, persistent = FALSE, defaultValues = NULL,
                      color = NULL, opacityDim = 0.2, showInLegend = FALSE) {
  if (!is.plotly(p)) {
    stop("Don't know how to modify highlight options to objects of class:", class(p))
  }
  p <- plotly_build(p)
  keys <- unlist(lapply(p$x$data, "[[", "key"))
  if (length(keys) == 0) {
    warning("No 'key' attribute found. \n", 
            "Linked interaction(s) aren't possible without a 'key' attribute.",
            call. = FALSE)
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
  p$x$highlight <- modify_list(
    p$x$highlight,
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
  # set some default crosstalk selections, if appropriate
  defaultValues <- defaultValues[defaultValues %in% keys]
  if (length(defaultValues)) {
    sets <- lapply(p$x$data, "[[", "set")
    for (i in seq_along(sets)) {
      valsInSet <- defaultValues[defaultValues %in% p$x$data[[i]][["key"]]]
      if (!length(valsInSet)) next
      p <- htmlwidgets::onRender(p, sprintf("
        function(el, x) {
          crosstalk.group('%s').var('selection').set(%s)
        }", sets[i], jsonlite::toJSON(valsInSet, auto_unbox = FALSE)))
    }
  }
  if (dynamic) {
    w <- colourpicker::colourWidget(
      value = color[1],
      palette = "limited",
      allowedCols = color
    )
    w <- htmlwidgets::onRender(w, "
      function(el, x) {
        var $el = $('#' + el.id);
        crosstalk.var('plotlySelectionColour').set($el.colourpicker('value'));
        $el.on('change', function() {
          crosstalk.var('plotlySelectionColour').set($el.colourpicker('value'));
        })
      }")
    p <- htmltools::browsable(htmltools::tagList(w, p))
  }
  p
}


highlight_defaults <- function() {
  formals(highlight)[-1]
}
