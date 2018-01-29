sf_fortify <- function(model, ...) {
  # TODO: 
  # (1) avoid converting redundant features
  # (2) warn/error if data already contains x/y 
  geoms <- sf::st_geometry(sf::st_as_sf(model))
  xy <- lapply(geoms, st_as_plotly)
  ids <- rep(seq_len(nrow(model)), sapply(xy, nrow))
  xy_all <- cbind(do.call(rbind, xy), ids)
  sf_key_name <- ".sf-group-id"
  xy_dat <- setNames(as.data.frame(xy_all), c("x", "y", sf_key_name))
  
  d <- as.data.frame(model)
  d$geometry <- NULL
  d[[sf_key_name]] <- seq_len(nrow(d))
  xy_dat <- dplyr::left_join(xy_dat, d, by = sf_key_name)
  xy_dat[[sf_key_name]] <- NULL
  xy_dat
}

# kind of like sf_as_grob(), but maps to a plotly data structure, rather than a grob
st_as_plotly <- function(x, ...) {
  UseMethod("st_as_plotly")
}

#' @export
st_as_plotly.POINT = function(x, ...) {
  matrix(c(x[1], x[2]), ncol = 2)
}

#' @export
st_as_plotly.MULTIPOINT = function(x, ...) {
  if (nrow(x) == 0) return(empty_xy())
  matrix(c(x[, 1], x[, 2]), ncol = 2)
}

#' @export
st_as_plotly.LINESTRING = function(x, ...) {
  if (nrow(x) == 0) return(empty_xy())
  matrix(c(x[, 1], x[, 2]), ncol = 2)
}

#' @export
st_as_plotly.CIRCULARSTRING = function(x, y, ...) {
  st_as_plotly(st_cast(x, "LINESTRING"),  ...)
}

#' @export
st_as_plotly.MULTILINESTRING = function(x, ...) {
  if (length(x) == 0) return(empty_xy())
  xvals <- unlist(rbind(sapply(x, function(y) y[,1]), NA))
  yvals <- unlist(rbind(sapply(x, function(y) y[,2]), NA))
  matrix(c(xvals, yvals), ncol = 2)
}

#' @export
st_as_plotly.POLYGON = function(x, ..., rule = "evenodd") {
  if (length(x) == 0) return(empty_xy())
  xvals <- unlist(rbind(sapply(x, function(y) y[,1]), NA))
  yvals <- unlist(rbind(sapply(x, function(y) y[,2]), NA))
  matrix(c(xvals, yvals), ncol = 2)
}

#' @export
st_as_plotly.MULTIPOLYGON = function(x, ..., rule = "evenodd") {
  if (length(x) == 0) return(empty_xy())
  xvals <- unlist(sapply(x, function(y) rbind(sapply(y, function(z) z[,1]), NA)))
  yvals <- unlist(sapply(x, function(y) rbind(sapply(y, function(z) z[,2]), NA)))
  matrix(c(xvals, yvals), ncol = 2)
}

#' @export
st_as_plotly.GEOMETRYCOLLECTION = function(x, ...) {
  if (length(x) == 0) return(empty_xy())
  lapply(x, st_as_plotly)
}

#' @export
st_as_plotly.MULTISURFACE = st_as_plotly.GEOMETRYCOLLECTION

#' @export
st_as_plotly.CURVEPOLYGON = st_as_plotly.GEOMETRYCOLLECTION

#' @export
st_as_plotly.COMPOUNDCURVE = st_as_plotly.GEOMETRYCOLLECTION



empty_xy <- function() {
  matrix(rep(NA, 2), ncol = 2)
}
