# take a "tidy" sf data frame and "expand it" so every row represents a point
# rather than geometry/geometries
expand <- function(data) {
  xs <- lapply(data$geometry, get_x)
  ys <- lapply(data$geometry, get_y)
  ns <- lapply(data$geometry, get_l)
  ids <- lapply(ns, function(x) rep(seq_len(length(x)), x))
  dats <- Map(function(x, y, z, w) {
    data.frame(x, y, group = paste(z, w, sep = "-"), `.plotlyMergeID` = w, stringsAsFactors = FALSE)
  }, xs, ys, ids, seq_along(data$geometry))
  # merge this "expanded" geometry data back with original data
  ids <- seq_len(nrow(data))
  data[[".plotlyMergeID"]] <- ids
  data[["group"]] <- NULL
  dplyr::left_join(dplyr::bind_rows(dats), data, by = ".plotlyMergeID")
}

# ------------------------------------------------------------------
# these helper functions are adapted from methods(st_as_grob)
# see, for example, getS3method("st_as_grob", "MULTIPOLYGON")
# ------------------------------------------------------------------

#' Obtain x coordinates of sf geometry/geometries
#' 
#' Exported for internal reasons. Not intended for general use.
#' 
#' @param g an sf geometry
#' @export
get_x <- function(g) {
  UseMethod("get_x")
}

#' Obtain y coordinates of sf geometry/geometries
#' 
#' Exported for internal reasons. Not intended for general use.
#' 
#' @param g an sf geometry
#' @export
get_y <- function(g) {
  UseMethod("get_y")
}

#' Obtain number of points comprising a geometry
#' 
#' Exported for internal reasons. Not intended for general use.
#' 
#' @param g an sf geometry
#' @export
get_l <- function(g) {
  UseMethod("get_l")
}

#' @export
get_x.MULTIPOLYGON <- function(g) {
  unlist(sapply(g, function(v) sapply(v, function(z) z[, 1])))
}

#' @export
get_y.MULTIPOLYGON <- function(g) {
  unlist(sapply(g, function(v) sapply(v, function(z) z[, 2])))
}

#' @export
get_l.MULTIPOLYGON <- function(g) {
  unlist(sapply(g, function(v) sapply(v, nrow)))
}

#' @export
get_x.POLYGON <- function(g) {
  unlist(sapply(g, function(y) y[, 1]))
}

#' @export
get_y.POLYGON <- function(g) {
  unlist(sapply(g, function(y) y[, 2]))
}

#' @export
get_l.POLYGON <- function(g) {
  sapply(g, nrow)
}

#' @export
get_x.MULTILINESTRING <- function(g) {
  unlist(sapply(g, function(y) y[, 1]))
}

#' @export
get_y.MULTILINESTRING <- function(g) {
  unlist(sapply(g, function(y) y[, 2]))
}

#' @export
get_l.MULTILINESTRING <- function(g) {
  sapply(g, nrow)
}

#' @export
get_x.LINESTRING <- function(g) {
  g[, 1]
}

#' @export
get_y.LINESTRING <- function(g) {
  g[, 2]
}

#' @export
get_l.LINESTRING <- function(g) {
  nrow(g)
}

#' @export
get_x.MULTIPOINT <- function(g) {
  g[, 1]
}

#' @export
get_y.MULTIPOINT <- function(g) {
  g[, 2]
}

#' @export
get_l.MULTIPOINT <- function(g) {
  nrow(g)
}

#' @export
get_x.POINT <- function(g) {
  g[1]
}

#' @export
get_y.POINT <- function(g) {
  g[2]
}

#' @export
get_l.POINT <- function(g) {
  nrow(g)
}
