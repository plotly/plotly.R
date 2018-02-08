# IMPORTANT: fortify_sf() should only be used on collections of identical 
# geometries -- https://github.com/r-spatial/sf/issues/584

# Note how, in to_basic.GeomSf(), we split data into a list of data.frames
# (each data frame has at most one geometry type).

# TODO: 
# (1) apply these same crs warnings/conversions to mapbox! https://github.com/rstudio/leaflet/blame/d489e2cd/R/normalize-sf.R#L94-L113
# (2) speed this up -- avoid converting redundant features
fortify_sf <- function(model, ...) {
  # matrix with coordinates (X, Y, possibly Z and/or M) in rows, possibly 
  # followed by integer indicators L1,...,L3 that point out to which structure 
  # the coordinate belongs; for POINT this is absent (each coordinate is a feature), 
  # for LINESTRING L1 refers to the feature, for MULTIPOLYGON L1 refers to the main 
  # ring or holes, L2 to the ring id in the MULTIPOLYGON, and L3 to the simple feature.
  coords <- sf::st_coordinates(model$geometry)
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
  coords <- tibble::as_tibble(coords)
  coords[[sf_key()]] <- coords[[ncol(coords)]]
  
  # join back together
  d <- dplyr::left_join(coords, model, by = sf_key())
  
  # retrain grouping variable (if it exists)
  d$group <- paste(d$group, Reduce(paste0, d[names(d) %in% c("l1", "l2", "l3")]), sep = "-")
  d
}

sf_key <- function() ".sf-group-id"
