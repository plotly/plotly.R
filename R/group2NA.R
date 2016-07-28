
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

group2NA <- function(data, groupNames = "group", nested = NULL, ordered = NULL,
                     retrace.first = inherits(data, "GeomPolygon")) {
  if (NROW(data) == 0) return(data)
  data <- data[!duplicated(names(data))]
  # a few workarounds since dplyr clobbers classes that we rely on in ggplotly
  retrace <- force(retrace.first)
  datClass <- class(data)
  
  # sanitize variable names
  groupNames <- groupNames[groupNames %in% names(data)]
  nested <- nested[nested %in% names(data)]
  ordered <- ordered[ordered %in% names(data)]
  # ignore any already existing groups
  data <- dplyr::ungroup(data)
  
  # if group doesn't exist, just arrange before returning
  if (!length(groupNames)) {
    if (length(ordered)) {
      data <- dplyr::arrange_(data, c(nested, ordered))
    }
    return(data)
  }
  allVars <- c(nested, groupNames, ordered)
  for (i in allVars) {
    data <- dplyr::group_by_(data, i, add = TRUE)
  }
  # first, arrange everything
  data <- dplyr::do(data, dplyr::arrange_(., allVars))
  data <- dplyr::ungroup(data)
  for (i in c(nested, groupNames)) {
    data <- dplyr::group_by_(data, i, add = TRUE)
  }
  d <- if (retrace.first) {
    dplyr::do(data, rbind(., .[1,], NA))
  } else {
    dplyr::do(data, rbind(., NA))
  }
  # TODO: how to drop the NAs separating the nested values? Does it even matter?
  # d <- dplyr::ungroup(d)
  # for (i in nested) {
  #   d <- dplyr::group_by_(dplyr::ungroup(d), i, add = TRUE)
  # }
  # d <- dplyr::do(d, .[seq_len(NROW(.)),])
  n <- NROW(d)
  if (all(is.na(d[n, ]))) d <- d[-n, ]
  structure(d, class = datClass)
}


# to appease R CMD check (currently we reference '.' in group2NA)
utils::globalVariables(".")
