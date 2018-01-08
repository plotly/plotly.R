# kind of like sf_as_plotly(), but maps to a plotly data structure, rather than a grob
sf_as_plotly <- function(row, ...) {
  UseMethod("st_as_plotly")
}

#' @export
sf_as_plotly.POINT = function(row, ...) {
  x <- row[1] 
  y <- row[2]
  x
}

#' @export
sf_as_plotly.MULTIPOINT = function(row, ...) {
  if (nrow(x) == 0)
    nullGrob()
  else
    pointsGrob(x[,1], x[,2], ..., default.units = default.units)
}

#' @export
sf_as_plotly.LINESTRING = function(row, ...) {
  if (nrow(x) == 0)
    nullGrob()
  else
    linesGrob(x[,1], x[,2], ..., default.units = default.units)
}

#' @export
sf_as_plotly.CIRCULARSTRING = function(x, y, ...) {
  sf_as_plotly(st_cast(x, "LINESTRING"),  ...)
}

#' @export
sf_as_plotly.MULTILINESTRING = function(row, ...) {
  if (length(x) == 0)
    nullGrob()
  else {
    get_x = function(x) unlist(sapply(x, function(y) y[,1]))
    get_y = function(x) unlist(sapply(x, function(y) y[,2]))
    polylineGrob(get_x(x), get_y(x), id.lengths = vapply(x, nrow, 0L), ...,
                 default.units = default.units)
  }
}

#' @export
sf_as_plotly.POLYGON = function(row, ..., rule = "evenodd") {
  if (length(x) == 0)
    nullGrob()
  else {
    get_x = function(x) unlist(sapply(x, function(y) y[,1]))
    get_y = function(x) unlist(sapply(x, function(y) y[,2]))
    pathGrob(get_x(x), get_y(x), id.lengths = vapply(x, nrow, 0L), ..., default.units = default.units, rule = rule)
  }
}

#' @export
sf_as_plotly.MULTIPOLYGON = function(row, ..., rule = "evenodd") {
  if (length(x) == 0)
    nullGrob()
  else {
    get_x = function(x) unlist(sapply(x, function(y) sapply(y, function(z) z[,1])))
    get_y = function(x) unlist(sapply(x, function(y) sapply(y, function(z) z[,2])))
    get_l = function(x) unlist(sapply(x, function(y) vapply(y, nrow, 0L)))
    pathGrob(get_x(x), get_y(x), id.lengths = get_l(x), ..., default.units = default.units, rule = rule)
  }
}

#' @export
sf_as_plotly.GEOMETRYCOLLECTION = function(row, ...) {
  if (length(x) == 0)
    nullGrob()
  else
    do.call(grid::grobTree, lapply(x, sf_as_plotly, ..., default.units = default.units))
}

#' @export
sf_as_plotly.MULTISURFACE = sf_as_plotly.GEOMETRYCOLLECTION

#' @export
sf_as_plotly.CURVEPOLYGON = sf_as_plotly.GEOMETRYCOLLECTION

#' @export
sf_as_plotly.COMPOUNDCURVE = sf_as_plotly.GEOMETRYCOLLECTION
