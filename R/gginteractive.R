#' @include ggplotly.R
#' @rdname ggplotly
#' @importFrom purrr partial
#' 
#' @param interactive
#'  If `TRUE` (default), then ggplot object is converted to plotly object.
#' 
#' @export
gginteractive <- function(
  interactive = TRUE, width = NULL, height = NULL, tooltip = "all",
  dynamicTicks = FALSE, layerData = 1, originalData = TRUE, source = "A", ...
) {
  structure(
    if (interactive) {
      partial(
        ggplotly,
        width = width, height = height, tooltip = tooltip,
        dynamicTicks = dynamicTicks, layerData = layerData,
        originalData = originalData, source = source, ...
      )
    } else {
      identity
    },
    class = c("gginteractive", "function")
  )
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.gginteractive <- function(object, plot, object_name) object(plot)
