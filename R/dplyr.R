#' Insert missing values to create trace groupings
#' 
#' If a group of traces share the same non-positional characteristics (i.e.,
#' color, fill, etc), it is more efficient to draw them as a single trace 
#' with missing values that separate the groups (instead of multiple traces).
#' This is a helper function for inserting missing values into a data set
#' 
#' @param data a data frame.
#' @param groupName the name of the grouping variable as a character string
#' @param nested other variables that group should be nested 
#' (i.e., ordered) within.
#' @param retrace.first should the first row of each group be appended to the 
#' last row? This is useful for enclosing polygons with lines.
#' @examples 
#' elong <- tidyr::gather(economics, variable, value, -date)
#' plot_ly(group2NA(elong, "variable"), x = ~date, y = ~value)
#' 

group2NA <- function(data, groupName = "group", nested = NULL, 
                     retrace.first = inherits(data, "GeomPolygon")) {
  if (nrow(data) == 0 || any(!groupName %in% names(data))) return(data)
  nested <- nested[nested %in% names(data)]
  data <- data[do.call(order, data[c(nested, groupName)]), , drop = FALSE]
  s <- split(data, data[[groupName]], drop = TRUE)
  f <- if (retrace.first) {
    function(x) rbind(x, x[1, , drop = FALSE], NA)
  } else {
    function(x) rbind(x, NA)
  }
  dplyr::bind_rows(lapply(s, f))
}
