
# Insert missing values to create trace groupings
# 
# If a group of traces share the same non-positional characteristics (i.e.,
# color, fill, etc), it is more efficient to draw them as a single trace 
# with missing values that separate the groups (instead of multiple traces).
# This is a helper function for inserting missing values into a data set
# 
# @param data a data frame.
# @param groupNames name(s) of the grouping variable(s) as a character vector
# @param nested other variables that group should be nested 
# (i.e., ordered) within.
# @param ordered a variable to arrange by (within nested & groupNames). This
# is useful primarily for ordering by x
# @param retrace.first should the first row of each group be appended to the 
# last row? This is useful for enclosing polygons with lines.
# @examples 
# 
# group2NA(mtcars, "vs", "cyl")
# 
# elong <- tidyr::gather(economics, variable, value, -date)
# plot_ly(group2NA(elong, "variable"), x = ~date, y = ~value)
# 
# @import data.table
#

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
  
  dt <- data.table::setDT(data)
  
  # if group doesn't exist, just order the rows and exit
  if (!length(groupNames)) {
    # TODO: what if nested doesn't exist?
    if (length(ordered)) data.table::setorderv(dt, cols = c(nested, ordered))
    return(
      structure(data.table::setDF(dt), class = datClass)
    )
  }
  # order the rows
  data.table::setorderv(dt, cols = c(nested, groupNames, ordered))
  
  # retracing is useful for creating polygon(s) via scatter trace(s)
  if (retrace) {
    dt <- dt[, rbind(.SD, SD[1]), by = c(nested, groupNames)]
  }
  
  # when connectgaps=FALSE, inserting NAs ensures each "group" 
  # will be visually distinct https://plot.ly/r/reference/#scatter-connectgaps
  dt <- dt[, rbind(.SD, .SD[NA]), by = c(nested, groupNames)]
  
  # internally, nested really tracks trace index, meaning we don't need 
  # to seperate them
  if (length(nested)) {
    dt <- dt[, .SD[-.N], by = nested]
  }
  
  structure(data.table::setDF(dt), class = datClass)
}
