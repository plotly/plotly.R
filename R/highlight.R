#' Query graphical elements in multiple linked views
#' 
#' This function sets a variety of options for brushing (i.e., highlighting)
#' multiple plots. These options are primarily designed for linking
#' multiple plotly graphs, and may not behave as expected when linking 
#' plotly to another htmlwidget package via crosstalk. In some cases,
#' other htmlwidgets will respect these options, such as persistent selection
#' in leaflet (see `demo("highlight-leaflet", package = "plotly")`).
#' 
#' @param p a plotly visualization.
#' @param on turn on a selection on which event(s)? To disable on events 
#' altogether, use `NULL`. Currently the following are supported:
#' \itemize{
#'  \item `'plotly_click'`
#'  \item `'plotly_hover'`
#'  \item `'plotly_selected'`: triggered through rectangular 
#'  (layout.dragmode = 'select') or lasso (layout.dragmode = 'lasso') brush.
#' }
#' @param off turn off a selection on which event(s)? To disable off 
#' events altogether, use `NULL`. Currently the following are supported:
#' \itemize{
#'  \item `'plotly_doubleclick'`: triggered on a double mouse click while
#'  (layout.dragmode = 'zoom') or (layout.dragmode = 'pan')
#'  \item `'plotly_deselect'`: triggered on a double mouse click while 
#'  (layout.dragmode = 'select') or (layout.dragmode = 'lasso')
#'  \item `'plotly_relayout'`: triggered whenever axes are rescaled 
#'  (i.e., clicking the home button in the modebar) or whenever the height/width
#'  of the plot changes.
#' }
#' @param persistent should selections persist (i.e., accumulate)? We often
#' refer to the default (`FALSE`) as a 'transient' selection mode; 
#' which is recommended, because one may switch from 'transient' to 
#' 'persistent' selection by holding the shift key.
#' @param dynamic should a widget for changing selection colors be included? 
#' @param color character string of color(s) to use for 
#' highlighting selections. See [toRGB()] for valid color
#' specifications. If `NULL` (the default), the color of selected marks
#' are not altered.
#' @param selectize provide a selectize.js widget for selecting keys? Note that 
#' the label used for this widget derives from the groupName of the SharedData object.
#' @param defaultValues a vector of values for setting a "default selection".
#' These values should match the key attribute.
#' @param opacityDim a number between 0 and 1 used to reduce the
#' opacity of non-selected traces (by multiplying with the existing opacity).
#' @param selected attributes of the selection, see [attrs_selected()].
#' @param debounce amount of time to wait before firing an event (in milliseconds).
#' The default of 0 means do not debounce at all. 
#' Debouncing is mainly useful when `on = "plotly_hover"` to avoid firing too many events
#' when users clickly move the mouse over relevant graphical marks.
#' @param ... currently not supported.
#' @export
#' @author Carson Sievert
#' @references \url{https://cpsievert.github.io/plotly_book/linking-views-without-shiny.html}
#' @seealso [attrs_selected()]
#' @examples
#' 
#' # These examples are designed to show you how to highlight/brush a *single*
#' # view. For examples of multiple linked views, see `demo(package = "plotly")` 
#' 
#' library(crosstalk)
#' d <- highlight_key(txhousing, ~city)
#' p <- ggplot(d, aes(date, median, group = city)) + geom_line()
#' gg <- ggplotly(p, tooltip = "city") 
#' highlight(gg, dynamic = TRUE)
#' 
#' # supply custom colors to the brush 
#' cols <- toRGB(RColorBrewer::brewer.pal(3, "Dark2"), 0.5)
#' highlight(gg, on = "plotly_hover", color = cols, dynamic = TRUE)
#' 
#' # Use attrs_selected() for complete control over the selection appearance
#' # note any relevant colors you specify here should override the color argument
#' s <- attrs_selected(
#'   showlegend = TRUE,
#'   mode = "lines+markers",
#'   marker = list(symbol = "x")
#' )
#' 
#' highlight(layout(gg, showlegend = TRUE), selected = s)
#' 

highlight <- function(p, on = "plotly_click", off, 
                      persistent = getOption("persistent", FALSE),
                      dynamic = FALSE, color = NULL,
                      selectize = FALSE, defaultValues = NULL,
                      opacityDim = getOption("opacityDim", 0.2), 
                      selected = attrs_selected(), debounce = 0,
                      ...) {
  
  # currently ... is not-supported and will catch 
  # some arguments we supported at one point 
  dots <- list(...)
  if (length(dots)) {
    warning(
      "The following arguments are not supported:\n",
      toString(names(dots)), "\n",
      "Arguments such as: hoverinfo and showInLegend \n",
      "have been replaced by selected and other",
      call. = FALSE
    )
  }
  
  if (opacityDim < 0 || 1 < opacityDim) {
    stop("opacityDim must be between 0 and 1", call. = FALSE)
  }
  if (dynamic && length(color) < 2) {
    message("Adding more colors to the selection color palette.")
    color <- c(color, RColorBrewer::brewer.pal(4, "Set1"))
  }
  if (!dynamic && length(color) > 1) {
    warning(
      "Can only use a single color for selections when `dynamic = FALSE`.",
      call. = FALSE
    )
    color <- color[1] 
  }
  
  # attach HTML dependencies (these libraries are used in the HTMLwidgets.renderValue() method)
  # TODO: only attach these when keys are present!
  if (selectize) {
    p$dependencies <- c(p$dependencies, list(selectizeLib()))
  }
  if (dynamic) {
    p$dependencies <- c(p$dependencies, list(colourPickerLib()))
  }
  
  
  # TODO: expose unhover?
  off_options <- paste0(
    "plotly_", c("doubleclick", "deselect", "relayout")
  )
  if (missing(off)) {
    off_default <- switch(
      on %||% "", 
      plotly_selected = "plotly_deselect",
      plotly_click = "plotly_doubleclick",
      plotly_hover = "plotly_doubleclick"
    )
    off <- default(off_default %||% "plotly_relayout")
  }
  
  if (isTRUE(persistent)) {
    message(
      "We recommend setting `persistent` to `FALSE` (the default) because ",
      "persistent selection mode can now be used by holding the shift key ",
      "(while triggering the `on` event)."
    )
  }
  
  # main (non-plotly.js) spec passed along to HTMLwidgets.renderValue()
  p$x$highlight <- list(
    # NULL may be used to disable on/off events
    on = if (!is.null(on)) match.arg(on, paste0("plotly_", c("click", "hover", "selected"))),
    off = if (is.default(off)) off else if (!is.null(off)) match.arg(off, off_options),
    persistent = persistent,
    dynamic = dynamic,
    # TODO: convert to hex...see colourpicker:::formatHEX()
    color = toRGB(color),
    selectize = selectize,
    defaultValues = defaultValues,
    opacityDim = opacityDim,
    selected = selected,
    debounce = debounce
  )
  
  p
}

#' Specify attributes of selection traces
#' 
#' By default the name of the selection trace derives from the selected values.
#' 
#' 
#' @param opacity a number between 0 and 1 specifying the overall opacity of
#' the selected trace
#' @param ... other trace attributes attached to the selection trace.
#' @export
#' @author Carson Sievert

attrs_selected <- function(opacity = 1, ...) {
  if (opacity < 0 || 1 < opacity) {
    stop("opacity must be between 0 and 1", call. = FALSE)
  }
  
  args <- list(
    opacity = opacity
  )
  
  # TODO: verify attr names... maybe that should happen in the build step?
  dots <- list(...)
  
  
  c(dots, args)
}


# ----------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------


highlight_defaults <- function() {
  args <- formals(highlight)[-1]
  # have to evaluate args now that some of them are functions...
  compact(lapply(args, function(x) tryNULL(eval(x))))
}

selectizeLib <- function(bootstrap = TRUE) {
  htmltools::htmlDependency(
    "selectize", "0.12.0", depPath("selectize"),
    stylesheet = if (bootstrap) "selectize.bootstrap3.css",
    script = "selectize.min.js"
  )
}

colourPickerLib <- function() {
  htmltools::htmlDependency(
    "colourpicker", "1.1", depPath("colourpicker"),
    stylesheet = "colourpicker.min.css",
    script = "colourpicker.min.js"
  )
}

depPath <- function(...) {
  system.file('htmlwidgets', 'lib', ..., package = 'plotly')
}
