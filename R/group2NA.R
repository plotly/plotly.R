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
#' is set to `FALSE`.
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
#' @return a data.frame with rows ordered by: `nested`, 
#' then `groupNames`, then `ordered`. As long as `groupNames` 
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
  
  # for restoring class information on exit
  datClass <- oldClass(data)
  
  # data.table doesn't play nice with list-columns
  if (inherits(data, "sf")) data <- fortify_sf(data)
  
  # evaluate this lazy argument now (in case we change class of data)
  retrace <- force(retrace.first)
  
  # sanitize variable names (TODO: throw warnings if non-existing vars are referenced?)
  groupNames <- groupNames[groupNames %in% names(data)]
  nested <- nested[nested %in% names(data)]
  ordered <- ordered[ordered %in% names(data)]
  
  dt <- data.table::as.data.table(data)
  
  # if group doesn't exist, just order the rows and exit
  if (!length(groupNames)) {
    keyVars <- c(nested, ordered)
    if (length(keyVars)) data.table::setorderv(dt, cols = keyVars)
    return(structure(dt, class = datClass))
  }
  
  # order the rows
  data.table::setorderv(dt, cols = c(nested, groupNames, ordered))
  
  # when connectgaps=FALSE, inserting NAs ensures each "group" 
  # will be visually distinct https://plot.ly/r/reference/#scatter-connectgaps
  # also, retracing is useful for creating polygon(s) via scatter trace(s)
  keyVars <- c(nested, groupNames)
  keyNum <- length(keyVars) + 1
  idx <- if (retrace) {
    dt[, c(.I, .I[1], NA), by = keyVars][[keyNum]]
  } else {
    dt[, c(.I, NA), by = keyVars][[keyNum]]
  }
  dt <- dt[idx]
  
  # remove NAs that unnecessarily seperate nested groups
  # (at least internally, nested really tracks trace index, meaning we don't need 
  # to seperate them)
  NAidx <- which(is.na(idx))
  for (i in seq_along(keyVars)) {
    dt[[keyVars[[i]]]][NAidx] <- dt[[keyVars[[i]]]][NAidx - 1]
  }
  if (length(nested)) {
    dt <- dt[ dt[, .I[-.N], by = nested][[length(nested) + 1]] ]
  } else {
    dt <- dt[-.N]
  }

  structure(dt, class = datClass)
}

utils::globalVariables(c(".I", ".N"))
