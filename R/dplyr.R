#' Dplyr verbs for plotly.
#'
#' @name plotly-ggvis
#' @keywords internal
#' @examples
#' 
#' library(dplyr)
#' plot_ly(mtcars, x = ~wt, y = ~mpg) %>%
#'   filter(cyl == 4) %>%
#'   add_points()
#' 
NULL

#' Divide data into groups.
#'
#' @param p a plotly visualization.
#' @param ... variables to group by.
#' @param add By default, when \code{add = FALSE}, \code{group_by} will
#'   override existing groups. To instead add to the existing groups,
#'   use \code{add = TRUE}
#' @importFrom dplyr group_by
#' @name group_by
#' @export
NULL

#' @export
#' @rdname dplyr-plotly
groups.plotly <- function(x) {
  dplyr::groups(plotly_data(x))
}

#' @export
#' @rdname dplyr-plotly
group_by_.plotly <- function(.data, ..., .dots, add = FALSE) {
  d <- plotly_data(.data)
  d <- dplyr::group_by_(d, .dots = lazyeval::all_dots(.dots, ...), add = add)
  # TODO: where to use group2NA? In plotly_build()
  add_data(.data, d)
}

#' @export
#' @rdname dplyr-plotly
ungroup.plotly <- function(x) {
  dplyr::ungroup(plotly_data(x))
}


#' @rdname dplyr-plotly
#' @export
summarise_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::summarise_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname dplyr-plotly
#' @export
mutate_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::mutate_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname dplyr-plotly
#' @export
arrange_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::arrange_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname dplyr-plotly
#' @export
select_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::select_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname dplyr-plotly
#' @export
filter_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::filter_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname dplyr-plotly
#' @export
distinct_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::distinct_(d, ..., .dots, add = add)
  add_data(.data, d)
}

#' @rdname dplyr-plotly
#' @export
slice_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::slice_(d, ..., .dots, add = add)
  add_data(.data, d)
}

#' @rdname dplyr-plotly
#' @export
rename_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::rename_(d, ..., .dots, add = add)
  add_data(.data, d)
}

#' @rdname dplyr-plotly
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
