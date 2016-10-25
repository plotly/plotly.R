#' Highlight graphical elements in multiple linked views
#' 
#' For documentation and examples, see 
#' \url{https://cpsievert.github.io/plotly_book/linking-views-without-shiny.html}
#' 
#' @param p a plotly visualization.
#' @param on turn on a selection on which event(s)? Likely candidates are
#' 'plotly_hover', 'plotly_click', 'plotly_selected'.
#' @param off turn off a selection on which event(s)? Likely candidates are
#' 'plotly_unhover', 'plotly_doubleclick', 'plotly_deselect'.
#' @param persistent should selections persist (i.e., accumulate)?
#' @param dynamic should UI controls for changing selection colors be 
#' included in the output?
#' @param selectize provide a selectize.js widget for selecting keys? Note that 
#' the label used for this widget derives from the groupName of the SharedData object.
#' @param defaultValues a vector of values for setting a "default selection".
#' These values should match the key attribute.
#' @param color character string of color(s) to use for 
#' highlighting selections. See \code{\link{toRGB}()} for valid color
#' specifications. If \code{NULL} (the default), the color of selected marks
#' are not altered (only their opacity).
#' @param opacityDim a number between 0 and 1 used to reduce the
#' opacity of non-selected traces (by multiplying with the existing opacity).
#' @param hoverinfo hoverinfo attributes for the selected traces. The default,
#' \code{NULL}, means to inherit the hoverinfo attribute from the non-selected traces.
#' @param showInLegend populate an additional legend entry for the selection?
#' @export
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
#'   add_lines(text = ~city, hoverinfo = "text") %>%
#'   highlight(on = "plotly_hover", persistent = TRUE, selectize = TRUE)
#' 

highlight <- function(p, on = "plotly_selected", off = "plotly_relayout", 
                      persistent = FALSE, dynamic = FALSE, color = NULL,
                      selectize = FALSE, defaultValues = NULL,
                      opacityDim = 0.2, hoverinfo = NULL, showInLegend = FALSE) {
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
      hoverinfo = hoverinfo,
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
  
  # attach selectize dependencies/payloads to plot object and add selectize divs
  if (selectize) {
    p$dependencies <- c(p$dependencies, list(selectizeLib()))
    sets <- unlist(lapply(p$x$data, "[[", "set"))
    keys <- setNames(lapply(p$x$data, "[[", "key"), sets)
    uniqueSets <- unique(sets)
    for (i in uniqueSets) {
      k <- unique(keys[[i]])
      if (is.null(k)) next
      k <- k[!is.null(k)]
      id <- new_id()
      p$x$selectize[[id]] <- list(
        items = data.frame(value = k, label = k), group = i
      )
      d <- selectizeDIV(id, multiple = persistent, label = i)
      p <- htmltools::tagList(d, p)
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
    p <- htmltools::tagList(w, p)
  }
  
  htmltools::browsable(p)
}


highlight_defaults <- function() {
  formals(highlight)[-1]
}


# Heavily inspired by https://github.com/rstudio/crosstalk/blob/209ac2a2c0cb1e6e23ccec6c1bc1ac7b6ba17ddb/R/controls.R#L105-L125
selectizeDIV <- function(id, multiple = TRUE, label = NULL) {
  htmltools::tags$div(
    id = id, 
    class = "form-group crosstalk-input-plotly-highlight",
    htmltools::tags$label(class = "control-label", `for` = id, label),
    htmltools::tags$div(
      htmltools::tags$select(multiple = if (multiple) NA else NULL)
    )
  )
}

selectizeLib <- function(bootstrap = TRUE) {
  htmltools::htmlDependency(
    "selectize", "0.12.0", depPath("selectize"),
    stylesheet = if (bootstrap) "selectize.bootstrap3.css",
    script = "selectize.min.js"
  )
}

depPath <- function(...) {
  system.file('htmlwidgets', 'lib', ..., package = 'plotly')
}
