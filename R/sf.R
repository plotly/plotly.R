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
  coords <- as.data.frame(coords)
  coords[[sf_key()]] <- coords[[ncol(coords)]]
  
  # join back together
  d <- dplyr::left_join(coords, model, by = sf_key())
  
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
  
  # Don't have enough information to check
  if (is.na(crs)) return()
  
  isLongLat <- isTRUE(sf::st_is_longlat(x))
  isWGS84 <- grepl("+datum=WGS84", crs$proj4string, fixed = TRUE)
  
  if (!(isLongLat && isWGS84)) {
    warning(
      "The trace types 'scattermapbox' and 'scattergeo' require a coordinate ",
      "reference system (crs) that contains: '+proj=longlat +datum=WGS84', ",
      "but the crs provided is: '", crs$proj4string, "'. ",
      "Attempting transformation to the target coordinate system.",
      call. = FALSE
    )
    x <- sf::st_transform(x, '+proj=longlat +datum=WGS84')
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
