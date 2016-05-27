#' Insert missing values to create trace groupings
#' 
#' If a group of traces share the same non-positional characteristics (i.e.,
#' color, fill, etc), it is more efficient to draw them as a single trace 
#' with missing values that separate the groups (instead of multiple traces).
#' This is a helper function for inserting missing values into a data set
#' 
#' @param data a data frame.
#' @param groupNames name(s) of the grouping variable(s) as a character vector
#' @param nested other variables that group should be nested 
#' (i.e., ordered) within.
#' @param retrace.first should the first row of each group be appended to the 
#' last row? This is useful for enclosing polygons with lines.
#' @examples 
#' elong <- tidyr::gather(economics, variable, value, -date)
#' plot_ly(group2NA(elong, "variable"), x = ~date, y = ~value)
#' 

group2NA <- function(data, groupNames = "group", nested = NULL, 
                     retrace.first = inherits(data, "GeomPolygon")) {
  if (nrow(data) == 0) return(data)
  nested <- nested[nested %in% names(data)]
  if (length(nested)) {
    data <- dplyr::arrange_(data, nested)
  }
  d <- dplyr::group_by_(data, groupNames)
  d <- if (retrace.first) {
    dplyr::do(d, rbind(., .[1,], NA))
  } else {
    dplyr::do(d, rbind(., NA))
  }
  structure(tidyr::unnest(d), class = unique(class(data), class(d)))
}
