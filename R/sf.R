# IMPORTANT: this function should only be used on collections of identical 
# geometries -- https://github.com/r-spatial/sf/issues/584

# TODO: avoid converting redundant features
fortify_sf <- function(model, ...) {
  # TODO: this should only apply for scattermapbox....
  # sf_crs_check(model)
  
  # matrix with coordinates (X, Y, possibly Z and/or M) in rows, possibly 
  # followed by integer indicators L1,...,L3 that point out to which structure 
  # the coordinate belongs; for POINT this is absent (each coordinate is a feature), 
  # for LINESTRING L1 refers to the feature, for MULTIPOLYGON L1 refers to the main 
  # ring or holes, L2 to the ring id in the MULTIPOLYGON, and L3 to the simple feature.
  #browser()
  coords <- sf::st_coordinates(model$geometry)
  colnames(coords) <- tolower(colnames(coords))
  lcols <- grep("^l", colnames(coords))
  
  # no longer need to carry around geometry
  model$geometry <- NULL
  
  # no join necessary
  if (!length(lcols)) return(cbind(model, coords))
  
  # warn column name conflicts
  nms <- intersect(names(model), colnames(coords))
  if (length(nms)) {
    warning(
      sprintf("Found columns named: '%s' ", paste(nms, collapse = "', '")), 
      "in your data. These names conflict with auto-generated sf coordinates ",
      "it might be a good idea to change these names.",
      call. = FALSE
    )
  }
  
  # generate unique key for joining data with coordinates
  model[[sf_key()]] <- seq_len(nrow(model))
  
  # compute/attach a key for joining coordinates back with original data
  key <- rle(apply(coords[, lcols, drop = FALSE], 1, paste, collapse = "-"))
  coords <- tibble::as_tibble(coords)
  coords[[sf_key()]] <- rep(seq_along(key$lengths), key$lengths)
  
  # insert NAs according to 
  coords <- group2NA(coords, groupNames = sf_key())
  
  # join back together
  dplyr::left_join(coords, model, by = sf_key())
}

sf_key <- function() ".sf-group-id"

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
  browser()
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


# thanks Hadley Wickham https://github.com/rstudio/leaflet/blame/d489e2cd/R/normalize-sf.R#L94-L113
sf_crs_check <- function(x) {
  crs <- sf::st_crs(x)
  
  # Don't have enough information to check
  if (is.na(crs)) return()
  
  if (identical(sf::st_is_longlat(x), FALSE)) {
    warning("sf layer is not long-lat data", call. = FALSE)
  }
  
  if (!grepl("+datum=WGS84", crs$proj4string, fixed = TRUE)) {
    warning(
      "sf layer has inconsistent datum (", crs$proj4string, ").\n",
      "Need '+proj=longlat +datum=WGS84'",
      call. = FALSE
    )
  }
}
