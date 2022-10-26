#' @rawNamespace export(to_basic.GeomArc)
to_basic.GeomArc <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPath")
}
  
#' @rawNamespace export(to_basic.GeomArc0)
to_basic.GeomArc0 <- function(data, prestats_data, layout, params, p, ...) {
  # TODO: we should have another geom2trace method for curveGrob()...
  prefix_class(data, "GeomPath")
}

#' @rawNamespace export(to_basic.GeomArcBar)
to_basic.GeomArcBar <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPolygon")
}

#' @rawNamespace export(to_basic.GeomBezier0)
to_basic.GeomBezier0 <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPath")
}

#' @rawNamespace export(to_basic.GeomBspline0)
to_basic.GeomBspline0 <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPath")
}

#' @rawNamespace export(to_basic.GeomBsplineClosed0)
to_basic.GeomBsplineClosed0 <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPath")
}

#' @rawNamespace export(to_basic.GeomCircle)
to_basic.GeomCircle <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPolygon")
}

#' @rawNamespace export(to_basic.GeomMarkCircle)
to_basic.GeomMarkCircle <- function(data, prestats_data, layout, params, p, ...) {
  # TODO: need to incorporate logic from R/mark_circle.R
  stop("not yet implemented")
}

#' @rawNamespace export(to_basic.GeomMarkEllipsis)
to_basic.GeomMarkEllipsis <- function(data, prestats_data, layout, params, p, ...) {
  # TODO: need to incorporate logic from R/mark_elipsis.R
  stop("not yet implemented")
}

#' @rawNamespace export(to_basic.GeomMarkHull)
to_basic.GeomMarkHull <- function(data, prestats_data, layout, params, p, ...) {
  # TODO: need to incorporate logic from R/mark_hull.R
  stop("not yet implemented")
}

#' @rawNamespace export(to_basic.GeomMarkRect)
to_basic.GeomMarkRect <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPolygon")
}

#' @rawNamespace export(to_basic.GeomParallelSetsAxes)
to_basic.GeomParallelSetsAxes <- function(data, prestats_data, layout, params, p, ...) {
  browser()
  prefix_class(data, "GeomPolygon")
}

#' @rawNamespace export(to_basic.GeomPathInterpolate)
to_basic.GeomPathInterpolate <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPath")
}

#' @rawNamespace export(to_basic.GeomShape)
to_basic.GeomShape <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPolygon")
}

#' @rawNamespace export(to_basic.GeomSina)
to_basic.GeomSina <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPoint")
}
