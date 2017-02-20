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

# get x/y coordinates of simple feature geometries...
# these helper functions are adapted from methods(st_as_grob)
# see, for example, getS3method("st_as_grob", "MULTIPOLYGON")
get_x <- function(g) {
  UseMethod("get_x")
}

get_y <- function(g) {
  UseMethod("get_y")
}

get_l <- function(g) {
  UseMethod("get_l")
}

get_x.MULTIPOLYGON <- function(g) {
  unlist(sapply(g, function(v) sapply(v, function(z) z[, 1])))
}

get_y.MULTIPOLYGON <- function(g) {
  unlist(sapply(g, function(v) sapply(v, function(z) z[, 2])))
}

get_l.MULTIPOLYGON <- function(g) {
  unlist(sapply(g, function(v) sapply(v, nrow)))
}

get_x.POLYGON <- function(g) {
  unlist(sapply(g, function(y) y[, 1]))
}

get_y.POLYGON <- function(g) {
  unlist(sapply(g, function(y) y[, 2]))
}

get_l.POLYGON <- function(g) {
  sapply(g, nrow)
}

get_x.MULTILINESTRING <- function(g) {
  unlist(sapply(g, function(y) y[, 1]))
}

get_y.MULTILINESTRING <- function(g) {
  unlist(sapply(g, function(y) y[, 2]))
}

get_l.MULTILINESTRING <- function(g) {
  sapply(g, nrow)
}

get_x.LINESTRING <- function(g) {
  g[, 1]
}

get_y.LINESTRING <- function(g) {
  g[, 2]
}

get_l.LINESTRING <- function(g) {
  nrow(g)
}

get_x.MULTIPOINT <- function(g) {
  g[, 1]
}

get_y.MULTIPOINT <- function(g) {
  g[, 2]
}

get_l.MULTIPOINT <- function(g) {
  nrow(g)
}

get_x.POINT <- function(g) {
  g[1]
}

get_y.POINT <- function(g) {
  g[2]
}

get_l.POINT <- function(g) {
  nrow(g)
}
