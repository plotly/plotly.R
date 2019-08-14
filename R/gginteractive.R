#' @include ggplotly.R
#' @rdname ggplotly
#' @importFrom purrr partial
#' @export
gginteractive <- function(
  width = NULL, height = NULL, tooltip = "all", dynamicTicks = FALSE,
  layerData = 1, originalData = TRUE, source = "A", ...
) {
  structure(
    partial(
      ggplotly,
      width = width, height = height, tooltip = tooltip,
      dynamicTicks = dynamicTicks, layerData = layerData,
      originalData = originalData, source = source, ...
    ),
    class = c("gginteractive", "function")
  )
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.gginteractive <- function(object, plot, object_name) object(plot)
