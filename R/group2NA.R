#' Separate groups with missing values
#' 
#' This function is used internally by plotly, but may also be useful to some 
#' power users. The details section explains when and why this function is useful.
#' 
#' @details If a group of scatter traces share the same non-positional characteristics 
#' (i.e., color, fill, etc), it is more efficient to draw them as a single trace 
#' with missing values that separate the groups (instead of multiple traces), 
#' In this case, one should also take care to make sure 
#' \href{https://plot.ly/r/reference/#scatter-connectgaps}{connectgaps} 
#' is set to \code{FALSE}.
#' 
#' @param data a data frame.
#' @param groupNames character vector of grouping variable(s)
#' @param nested other variables that group should be nested 
#' (i.e., ordered) within.
#' @param ordered a variable to arrange by (within nested & groupNames). This
#' is useful primarily for ordering by x
#' @param retrace.first should the first row of each group be appended to the 
#' last row? This is useful for enclosing polygons with lines.
#' @export
#' @return a data.frame with rows ordered by: \code{nested}, 
#' then \code{groupNames}, then \code{ordered}. As long as \code{groupNames} 
#' contains valid variable names, new rows will also be inserted to separate 
#' the groups.
#' @examples 
#' 
#' # note the insertion of new rows with missing values 
#' group2NA(mtcars, "vs", "cyl")
#' 
#' # need to group lines by city somehow!
#' plot_ly(txhousing, x = ~date, y = ~median) %>% add_lines()
#' 
#' # instead of using group_by(), you could use group2NA()
#' tx <- group2NA(txhousing, "city")
#' plot_ly(tx, x = ~date, y = ~median) %>% add_lines()
#' 
#' # add_lines() will ensure paths are sorted by x, but this is equivalent
#' tx <- group2NA(txhousing, "city", ordered = "date")
#' plot_ly(tx, x = ~date, y = ~median) %>% add_paths()
#' 

group2NA <- function(data, groupNames = "group", nested = NULL, ordered = NULL,
                     retrace.first = inherits(data, "GeomPolygon")) {
  
  if (NROW(data) == 0) return(data)
  
  # evaluate this lazy argument now (in case we change class of data)
  retrace <- force(retrace.first)
  
  # sanitize variable names (TODO: throw warnings if non-existing vars are referenced?)
  groupNames <- groupNames[groupNames %in% names(data)]
  nested <- nested[nested %in% names(data)]
  ordered <- ordered[ordered %in% names(data)]
  
  # for restoring class information on exit
  datClass <- oldClass(data)
  
  data.table::setDT(data)
  
  # if group doesn't exist, just order the rows and exit
  if (!length(groupNames)) {
    if (length(ordered)) data.table::setorderv(data, cols = c(nested, ordered))
    return(structure(data, class = datClass))
  }
  
  # order the rows
  data.table::setorderv(data, cols = c(nested, groupNames, ordered))
  
  # retracing is useful for creating polygon(s) via scatter trace(s)
  if (retrace) {
    data <- data[, .SD[c(.I, 1)], by = c(nested, groupNames)]
  }
  
  # when connectgaps=FALSE, inserting NAs ensures each "group" 
  # will be visually distinct https://plot.ly/r/reference/#scatter-connectgaps
  data <- data[, .SD[c(.I, NA)], by = c(nested, groupNames)]
  
  # internally, nested really tracks trace index, meaning we don't need 
  # to seperate them
  data <- if (length(nested)) data[, .SD[-.N], by = nested] else data[-.N]
  
  structure(data, class = datClass)
}
