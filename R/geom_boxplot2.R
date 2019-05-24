#' Attempt to convert `geom_boxplot()` to a plotly.js box trace
#' 
#' There are two ways to create boxplot via [ggplotly()]: with either
#' this function or [ggplot2::geom_boxplot()]. This function uses
#' the [box](https://plot.ly/r/reference/#box) trace type whereas the 
#' latter uses a combination of [scatter](https://plot.ly/r/reference/#scatter) 
#' traces to render the visualization. This implies that, this 
#' function lets plotly.js compute boxplot summaries and positional 
#' dodging, whereas the latter uses the actual ggplot2 boxplot 
#' definition(s).
#' 
#' @param ... arguments passed along to [ggplot2::geom_boxplot()]
#' 
#' @export
#' @examples
#' 
#' subplot(
#'   ggplot(diamonds) + geom_boxplot(aes(y = price)),
#'   ggplot(diamonds) + geom_boxplot2(aes(y = price))
#' )
geom_boxplot2 <- function(...) {
  ggproto_box <- ggplot2::geom_boxplot(...)
  ggproto_box$plotlyGeomBoxplot2 <- TRUE
  ggproto_box
}
