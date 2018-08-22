# ---------------------------------------------------------------------------
# TODO: ggraph::GeomAxisHive
# ---------------------------------------------------------------------------



# ---------------------------------------------------------------------------
# ggraph custom Edge geoms
# ---------------------------------------------------------------------------

toEdgePath <- function(data, prestats_data, layout, params, p, ...) {
  names(data) <- sub("^edge_", "", names(data))
  prefix_class(data, "GeomPath")
}

#' @rawNamespace export(to_basic.GeomEdgeBezier)
to_basic.GeomEdgeBezier <- toEdgePath

#' @rawNamespace export(to_basic.GeomEdgeBspline)
to_basic.GeomEdgeBspline <- toEdgePath

#' @rawNamespace export(to_basic.GeomEdgeDensity)
to_basic.GeomEdgeDensity <- function(data, prestats_data, layout, params, p, ...) {
  # avoid a weird precision issue
  data$density[data$density < 0.005] <- 0
  data$fill_plotlyDomain <- data$density
  data$fill <- toRGB(
    data$edge_fill, scales::rescale(data$density)
  )
  prefix_class(data, "GeomTile")
}

#' @rawNamespace export(to_basic.GeomEdgePath)
to_basic.GeomEdgePath <- toEdgePath

#' @rawNamespace export(to_basic.GeomEdgeSegment)
to_basic.GeomEdgeSegment <- toEdgePath

#' @rawNamespace export(to_basic.GeomEdgePoint)
to_basic.GeomEdgePoint <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPoint")
}

# ---------------------------------------------------------------------------
# ggraph custom Node geoms
# ---------------------------------------------------------------------------

#' @rawNamespace export(to_basic.GeomNodeTile)
to_basic.GeomNodeTile <- function(data, prestats_data, layout, params, p, ...) {
  to_basic.GeomRect(data, prestats_data, layout, params, p, ...)
}
