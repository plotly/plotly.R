# IMPORTANT: fortify_sf() should only be used on collections of identical 
# geometries -- https://github.com/r-spatial/sf/issues/584

# Note how, in to_basic.GeomSf(), we split data into a list of data.frames
# (each data frame has at most one geometry type).

# TODO: speed this up by avoiding conversion redundant features (e.g. animation)
fortify_sf <- function(model, ...) {
  # matrix with coordinates (X, Y, possibly Z and/or M) in rows, possibly 
  # followed by integer indicators L1,...,L3 that point out to which structure 
  # the coordinate belongs; for POINT this is absent (each coordinate is a feature), 
  # for LINESTRING L1 refers to the feature, for MULTIPOLYGON L1 refers to the main 
  # ring or holes, L2 to the ring id in the MULTIPOLYGON, and L3 to the simple feature.
  coords <- sf::st_coordinates(sf::st_geometry(model))
  colnames(coords) <- tolower(colnames(coords))
  lcols <- grep("^l", colnames(coords))
  
  # no longer need to carry around sf class or geometry list-column
  model <- remove_class(model, "sf")
  model$geometry <- NULL
  
  # no join necessary for points
  if (!length(lcols)) return(cbind(model, coords))
  
  # warn about potential column name conflicts
  nms <- intersect(names(model), colnames(coords))
  if (length(nms)) {
    warning(
      sprintf("Found columns named: '%s' ", paste(nms, collapse = "', '")), 
      "in your data. These names conflict with auto-generated sf coordinates ",
      "it might be a good idea to change these names.",
      call. = FALSE
    )
  }
  
  # attach a simple feature row id (for joining data with coords)
  model[[sf_key()]] <- seq_len(nrow(model))
  
  # last column of coords pertains to the simple feature row id
  coords <- as.data.frame(coords)
  coords[[sf_key()]] <- coords[[ncol(coords)]]
  
  # join back together
  d <- cbind(coords, model[coords[[sf_key()]], setdiff(names(model), sf_key()), drop = FALSE])
  
  # drop simple feature row id
  d[[sf_key()]] <- NULL
  
  # the combination of l1/l2/l3 should be treated like a new grouping var
  # TODO: group is included here for ggplotly() purposes...it should probably be taken out...
  group_var <- d[names(d) %in% c("group", "l1", "l2", "l3")]
  paste_ <- function(...) paste(..., sep = "-")
  d$group <- Reduce(paste_, group_var)
  
  # TODO: drop l1/l2/l3?
  d
}

sf_key <- function() ".sf-group-id"

# Credit to Hadley Wickham 
# https://github.com/rstudio/leaflet/blame/d489e2cd/R/normalize-sf.R#L94-L113
st_cast_crs <- function(x) {
  crs <- sf::st_crs(x)
  
  if (is.na(crs)) {
    warning(
      "Missing coordinate reference system (crs). Defaulting to EPSG:4326"
    )
    return(sf::st_set_crs(x, 4326))
  }
  
  isLongLat <- isTRUE(sf::st_is_longlat(x))
  isWGS84 <- grepl("+datum=WGS84", crs$proj4string, fixed = TRUE)
  
  if (!(isLongLat && isWGS84)) {
    warning(
      "The trace types 'scattermapbox' and 'scattergeo' require a projected ",
      "coordinate system that is based on the WGS84 datum (EPSG:4326), ",
      "but the crs provided is: '", crs$proj4string, "'. ",
      "Attempting transformation to the target coordinate system.",
      call. = FALSE
    )
    return(sf::st_transform(x, 4326))
  }
  
  x
}

# the minimal set of attribute defaults
sf_default_attrs <- function(d) {
  if (inherits(d, "GeomPolygon")) return(list(mode = "lines", fill = "toself"))
  if (inherits(d, "GeomPath")) return(list(mode = "lines"))
  if (inherits(d, "GeomPoint")) return(list(mode = "markers"))
  stop("Unexpected case. Please report an issue here https://github.com/ropensci/plotly/issues/new", call. = FALSE)
}


is_sf <- function(dat) {
  if (crosstalk::is.SharedData(dat)) dat <- dat$origData()
  inherits(dat, "sf")
}

# The graticule of a coord_sf() panel, extended well beyond the data: plotly.js
# widens one axis range to keep the map's aspect ratio, so ggplot2's
# graticule (which covers only the panel's ranges) would stop short of the
# plot area's edges. The lines keep ggplot2's spacing and are clipped to the
# plot area by plotly.js. NULL when there is no graticule or it can't be
# computed (the panel's own graticule is used then).
extend_graticule <- function(rng, coord, k = 2) {
  g <- rng$graticule
  crs <- rng$crs
  datum <- coord$datum
  if (is.null(g) || !NROW(g) || is.null(crs) || is.na(crs)) return(NULL)
  if (is.null(datum) || (is.atomic(datum) && length(datum) == 1 && is.na(datum))) return(NULL)
  xr <- rng$x_range
  yr <- rng$y_range
  dx <- diff(xr)
  dy <- diff(yr)
  bb <- sf::st_bbox(
    c(xmin = xr[1] - k * dx, ymin = yr[1] - k * dy, xmax = xr[2] + k * dx, ymax = yr[2] + k * dy),
    crs = crs
  )
  tryCatch({
    # the extended area in the datum's degrees, to extend the breaks over:
    # from a grid of points over it (points outside the projection's valid
    # area don't transform, and are dropped)
    grid <- expand.grid(
      x = seq(bb[["xmin"]], bb[["xmax"]], length.out = 25),
      y = seq(bb[["ymin"]], bb[["ymax"]], length.out = 25)
    )
    pts <- sf::st_as_sf(grid, coords = c("x", "y"), crs = crs)
    xy <- sf::st_coordinates(suppressWarnings(sf::st_transform(pts, datum)))
    xy <- xy[is.finite(xy[, 1]) & is.finite(xy[, 2]), , drop = FALSE]
    if (!nrow(xy)) return(NULL)
    deg <- c(
      xmin = min(xy[, 1]), xmax = max(xy[, 1]),
      ymin = min(xy[, 2]), ymax = max(xy[, 2])
    )
    extend <- function(type, lo, hi, limit) {
      v <- sort(unique(g$degree[g$type == type]))
      if (length(v) < 2) return(v)
      step <- min(diff(v))
      # two steps beyond the area sampled (whose edges may lie outside the
      # projection's valid area, e.g. towards the poles); clipped anyway
      lo <- lo - 2 * step
      hi <- hi + 2 * step
      out <- seq(v[1] - ceiling((v[1] - lo) / step) * step, hi, by = step)
      out[out >= limit[1] & out <= limit[2]]
    }
    lon <- extend("E", deg[["xmin"]], deg[["xmax"]], c(-180, 180))
    lat <- extend("N", deg[["ymin"]], deg[["ymax"]], c(-90, 90))
    # meridians run between the outermost parallels, which st_graticule()
    # can't place at the poles: near them, so meridians reach the poles
    if (length(lat) > 1) {
      step <- min(diff(lat))
      if (deg[["ymax"]] + 2 * step > 90) lat <- c(lat[lat < 90], 89.999)
      if (deg[["ymin"]] - 2 * step < -90) lat <- c(-89.999, lat[lat > -90])
    }
    out <- sf::st_graticule(
      bb,
      crs = crs,
      datum = datum,
      lon = lon,
      lat = lat,
      ndiscr = coord$ndiscr %||% 100
    )
    if (NROW(out)) out else NULL
  }, error = function(e) NULL)
}
