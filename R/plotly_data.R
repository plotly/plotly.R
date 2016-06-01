#' View and manipulate data associated with a plotly graph
#'
#' \code{plotly_data()} returns the data associated with 
#' a plotly visualization. This data can be manipulated via 
#' the dplyr and tidyr verbs listed below:
#' 
#'
#' @name plotly_data
#' @keywords internal
#' @examples
#' 
#' # loading dplyr is super important!
#' library(dplyr)
#' 
#' mtcars %>%
#'   plot_ly(x = ~wt, y = ~mpg, name = "scatter trace") %>%
#'   filter(cyl == 4) %>%
#'   add_points(x = ~wt, y = ~mpg, name = "filtered points", 
#'     marker = list(color = "red"))
#'     
#' library(tidyr)
#' economics %>%
#'   gather(variable, value, -date) %>%
#'   group_by(variable) %>%
#'   plot_ly(x = ~date, y = ~value)
#'   
NULL

#' @export
#' @rdname plotly_data
plotly_data <- function(p, id = p$x$cur_data) {
  p$x$visdat[[id]]() %||% data.frame()
}

#' @export
#' @rdname plotly_data
groups.plotly <- function(x) {
  dplyr::groups(plotly_data(x))
}

#' @export
#' @rdname plotly_data
group_by_.plotly <- function(.data, ..., .dots, add = FALSE) {
  d <- plotly_data(.data)
  d <- dplyr::group_by_(d, .dots = lazyeval::all_dots(.dots, ...), add = add)
  # TODO: where to use group2NA? In plotly_build()
  add_data(.data, d)
}

#' @export
#' @rdname plotly_data
ungroup.plotly <- function(x) {
  dplyr::ungroup(plotly_data(x))
}

#' @rdname plotly_data
#' @export
summarise_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::summarise_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
mutate_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::mutate_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
arrange_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::arrange_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
select_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::select_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
filter_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::filter_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
distinct_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::distinct_(d, ..., .dots, add = add)
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
slice_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::slice_(d, ..., .dots, add = add)
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
rename_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::rename_(d, ..., .dots, add = add)
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
transmute_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::transmute_(d, ..., .dots, add = add)
  add_data(.data, d)
}

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
  d <- tidyr::unnest(d)
  n <- nrow(d)
  if (all(is.na(d[n, ]))) d <- d[-n, ]
  structure(d, class = unique(class(data), class(d)))
}
