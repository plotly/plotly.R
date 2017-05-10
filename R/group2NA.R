
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
  
  ## make copy and eliminate duplicated column names
  ## If data is already an internal copy and does not need to be protected from in place modifications, then 
  ## the copy being created here could be eliminated for all cases except where column names are duplicated -- 
  ## wouldn't save much time, but could lower amt. of memory allocation required for plotly calls.
  
  if(data.table::is.data.table(data)){
    data <- data[,unique(names(data)),with=FALSE]
  } else {
    data <- data[!duplicated(names(data))]
  }
  
  ## store class information from function input
  retrace <- force(retrace.first)
  datClass <- class(data)
  
  ## sanitize variable names
  groupNames <- groupNames[groupNames %in% names(data)]
  nested <- nested[nested %in% names(data)]
  ordered <- ordered[ordered %in% names(data)]
  
  ## if group doesn't exist, just arrange before returning
  if (!length(groupNames)) {
    if (length(ordered)) {
      return(
        structure(
          data.table::setDT(data,key = c(nested, ordered)),
          class = datClass)
      )
    } else {
      return(data)
    }
  }
  
  allVars <- c(nested, groupNames, ordered)
  
  ## if retrace.first is TRUE,repeat the first row of each group and add an empty row of NA's after each group.
  ## if retrace.first is FALSE, just add an empty row to each group.
  ## delete final row of NA's, return d with the original class
  
  ## IMPORTANT: does it matter if operating w/data.table setDT() clobbers row names attribute?
  if (retrace.first) {
    return(
      data.table::setDT(data, key = allVars)[ data[, .I[c(seq_along(.I), 1L, .N+1L)], by=allVars]$V1 ][-.N,] %>% 
        structure(class = datClass)
    )
  } else {
    return(
      structure(
        data.table::setDT(data, key = allVars)[ data[, .I[c(seq_along(.I), .N+1L)], by=allVars]$V1 ][-.N,],
        class = datClass)
    )
  }
  
  ## IMPORTANT: does this still need to be done?
  ## TODO: how to drop the NAs separating the nested values? Does it even matter?
  # d <- dplyr::ungroup(d)
  # for (i in nested) {
  #   d <- dplyr::group_by_(dplyr::ungroup(d), i, add = TRUE)
  # }
  # d <- dplyr::do(d, .[seq_len(NROW(.)),])
}


# to appease R CMD check (currently we reference '.' in group2NA)
utils::globalVariables(".")
