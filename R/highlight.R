#' Highlight graphical elements in multiple linked views
#' 
#' This function sets a variety of options for brushing (i.e., highlighting)
#' plotly graphs. Use this function to set options (or populate widgets) 
#' for a \emph{single} plot. When linking multiple plots, use 
#' \code{\link{options}()} to set "global" options, where the option name 
#' matches the relevant argument name. For instance, 
#' to link multiple plots with \code{persistent} selection, set
#' \code{options(persistent = TRUE)}. To see an example linking plotly to 
#' leaflet, see \code{demo("highlight-leaflet", package = "leaflet")}
#' 
#' @param p a plotly visualization.
#' @param on turn on a selection on which event(s)? Likely candidates are
#' 'plotly_hover', 'plotly_click', 'plotly_selected'. To disable on events 
#' altogether, use \code{NULL}.
#' @param off turn off a selection on which event(s)? Likely candidates are
#' 'plotly_unhover', 'plotly_doubleclick', 'plotly_deselect'. To disable off 
#' events altogether, use \code{NULL}.
#' @param persistent should selections persist (i.e., accumulate)?
#' @param dynamic should a widget for changing selection colors be included? 
#' @param color character string of color(s) to use for 
#' highlighting selections. See \code{\link{toRGB}()} for valid color
#' specifications. If \code{NULL} (the default), the color of selected marks
#' are not altered (only their opacity).
#' @param selectize provide a selectize.js widget for selecting keys? Note that 
#' the label used for this widget derives from the groupName of the SharedData object.
#' @param defaultValues a vector of values for setting a "default selection".
#' These values should match the key attribute.
#' @param opacityDim a number between 0 and 1 used to reduce the
#' opacity of non-selected traces (by multiplying with the existing opacity).
#' @param hoverinfo hoverinfo attributes for the selected traces. The default,
#' \code{NULL}, means to inherit the hoverinfo attribute from the non-selected traces.
#' @param showInLegend populate an additional legend entry for the selection?
#' @export
#' @author Carson Sievert
#' @references \url{https://cpsievert.github.io/plotly_book/linking-views-without-shiny.html}
#' @examples
#' 
#' library(crosstalk)
#' d <- SharedData$new(txhousing, ~city)
#' p <- ggplot(d, aes(date, median, group = city)) + geom_line()
#' ggplotly(p, tooltip = "city") %>%
#'   highlight(on = "plotly_hover", color = "red")
#'   
#' # The group name is currently used to populate a title for the selectize widget
#' sd <- SharedData$new(txhousing, ~city, "Choose a city")
#' plot_ly(sd, x = ~date, y = ~median) %>%
#'   group_by(city) %>%
#'   add_lines(text = ~city, hoverinfo = "text") %>%
#'   highlight(on = "plotly_hover", persistent = TRUE, selectize = TRUE)
#' 

highlight <- function(p, on = "plotly_selected", off = "plotly_relayout", 
                      persistent = FALSE, dynamic = FALSE, color = NULL,
                      selectize = FALSE, defaultValues = NULL,
                      opacityDim = 0.2, hoverinfo = NULL, showInLegend = FALSE) {
  if (opacityDim < 0 || 1 < opacityDim) {
    stop("opacityDim must be between 0 and 1", call. = FALSE)
  }
  if (dynamic && length(color) < 2) {
    message("Adding more colors to the selection color palette")
    color <- c(color, RColorBrewer::brewer.pal(4, "Set1"))
  }
  if (!dynamic && length(color) > 1) {
    warning(
      "Can only use a single color for selections when dynamic=FALSE",
      call. = FALSE
    )
    color <- color[1] 
  }
  # attach HTML dependencies (these libraries are used in the HTMLwidgets.renderValue() method)
  if (selectize) {
    p$dependencies <- c(p$dependencies, list(selectizeLib()))
  }
  if (dynamic) {
    p$dependencies <- c(p$dependencies, list(colourPickerLib()))
  }
  if (system.file(package = "rmarkdown") != "") {
    p$dependencies <- c(p$dependencies, list(rmarkdown::html_dependency_bootstrap("default")))
  } else {
    message("Install the rmarkdown package for nice font styling in widget labels ")
  }
  
  # main (non-plotly.js) spec passed along to HTMLwidgets.renderValue()
  p$x$highlight <- list(
    # NULL may be used to disable on/off events
    on = if (!is.null(on)) match.arg(on, paste0("plotly_", c("click", "hover", "selected"))),
    off = if (!is.null(off)) match.arg(off, paste0("plotly_", c("unhover", "doubleclick", "deselect", "relayout"))),
    persistent = persistent,
    dynamic = dynamic,
    # TODO: convert to hex...see colourpicker:::formatHEX()
    color = toRGB(color),
    selectize = selectize,
    defaultValues = defaultValues,
    opacityDim = opacityDim,
    hoverinfo = hoverinfo,
    showInLegend = showInLegend
  )
  
  p
}


highlight_defaults <- function() {
  formals(highlight)[-1]
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


# ----------------------------------------------------------------------------
# Artifacts from b4 we injected HTML content via JavaScript (so things "just work"
# in all contexts). Hopefully someday htmlwidgets::preprendContent() is 
# supported in shiny....
# ----------------------------------------------------------------------------

# 
# # Heavily inspired by https://github.com/rstudio/crosstalk/blob/209ac2a2c0cb1e6e23ccec6c1bc1ac7b6ba17ddb/R/controls.R#L105-L125
# selectizeDIV <- function(id, multiple = TRUE, label = NULL, width = "80%", height = "10%") {
#   htmltools::tags$div(
#     id = id, 
#     style = sprintf("width: %s; height: '%s'", width, height),
#     class = "form-group crosstalk-input-plotly-highlight",
#     htmltools::tags$label(class = "control-label", `for` = id, label),
#     htmltools::tags$div(
#       htmltools::tags$select(multiple = if (multiple) NA else NULL)
#     )
#   )
# }
# 
# # set argument relates to the "crosstalk group"
# colour_widget <- function(colors, set = new_id(), ...) {
#   
#   w <- colourpicker::colourWidget(
#     value = colors[1],
#     palette = "limited",
#     allowedCols = colors,
#     ...
#   )
#   
#   # inform crosstalk when the value of colour widget changes
#   htmlwidgets::onRender(w, sprintf("
#     function(el, x) {
#     var $el = $('#' + el.id);
#     var grp = crosstalk.group('%s').var('plotlySelectionColour')
#     grp.set($el.colourpicker('value'));
#     $el.on('change', function() {
#     crosstalk.group('%s').var('plotlySelectionColour').set($el.colourpicker('value'));
#     })
#   }", set, set))
#   
# }
