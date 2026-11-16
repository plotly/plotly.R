
test_that("geom_sf() basic polygons.", {
  skip_if_not_installed("sf")
  skip_if_not_installed("s2")
  
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  
  p <- ggplot(nc) + geom_sf()
  
  l <- expect_doppelganger_built(p, "sf")
  # one trace is for the graticule
  expect_length(l$data, 2)
  # can translate degree symbol
  expect_true(
    all(grepl("\\&#176;", l$layout$xaxis$ticktext))
  )
  expect_true(
    all(grepl("\\&#176;", l$layout$yaxis$ticktext))
  )
})

test_that("geom_sf() geometry collection.", {
  skip_if_not_installed("sf")
  
  # example from the sf vignette
  a <- sf::st_polygon(list(cbind(c(0,0,7.5,7.5,0),c(0,-1,-1,0,0))))
  b <- sf::st_polygon(list(cbind(c(0,1,2,3,4,5,6,7,7,0),c(1,0,.5,0,0,0.5,-0.5,-0.5,1,1))))
  i <- sf::st_intersection(a, b)
  cd <- sf::st_as_sf(data.frame(d = 1, geometry = sf::st_sfc(i)))
  
  p <- ggplot(cd) + geom_sf()
  l <- expect_doppelganger_built(p, "sf-geom-collection")
  
  # graticule, point, line, polygon
  expect_length(l$data, 4)
})

test_that("geom_sf() polygons with fill/text.", {
  skip_if_not_installed("sf")
  skip_if_not_installed("s2")
  
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- ggplot(nc) + geom_sf(aes(fill = AREA, text = NAME))
  
  l <- expect_doppelganger_built(p, "sf-fill-text")
  # one trace for every fillcolor, one for graticule, one for colorbar
  expect_length(l$data, length(unique(nc$AREA)) + 2)
  expect_true(
    all(unlist(lapply(l$data, "[[", "hoverinfo")) %in% c("skip", "none", "text"))
  )
  # graticule styling should inherit from panel.grid.major
  expect_equivalent(
    l$data[[1]]$line$color, 
    toRGB(ggplot2::calc_element("panel.grid.major", ggplot2::theme_gray())[["colour"]])
  )
})

test_that("geom_sf() with basic polygons and points.", {
  skip_if_not_installed("sf")
  skip_if_not_installed("s2")
  
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- ggplot(nc) +
    geom_sf() +
    annotate("point", x = -80, y = 35, colour = "red", size = 4) +
    theme(panel.grid.major = element_line(colour = "red"))
  
  l <- expect_doppelganger_built(p, "sf-points")
  # one trace for graticule, one for point, and one polygons
  expect_length(l$data, 3)
  # graticule should be red
  expect_equivalent(l$data[[1]]$line$color, "rgba(255,0,0,1)")
  expect_equivalent(l$data[[2]]$mode, "lines")
  expect_equivalent(l$data[[3]]$mode, "markers")
})

test_that("sf aspect ratio is correct", {
  skip_if_not_installed("sf")
  skip_if_not_installed("s2")
  
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- ggplot(nc) + geom_sf() 
  
  l <- expect_doppelganger_built(p, "sf-aspect")
  expect_equivalent(l$layout$xaxis$scaleanchor, "y")
  expect_equal(l$layout$xaxis$scaleratio, 0.81678435872298)
})


test_that("works with a blank theme", {
  skip_if_not_installed("sf")
  skip_if_not_installed("s2")
  skip_if_not_installed("ggthemes")
  
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- ggplot(nc) + geom_sf() + 
    ggthemes::theme_map()
  
  l <- expect_doppelganger_built(p, "sf-theme-map")
  
  # TODO: perhaps the graticule shouldn't be included at all?
  expect_length(l$data, 2)
  expect_equivalent(l$data[[1]]$line$color, "transparent")
})

test_that("resolves overlapping axis ticks", {
  skip_if_not_installed("sf")
  skip_if_not_installed("maps")
  
  world <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
  
  # filter the world sf object down to canada
  canada <- dplyr::filter(world, ID == "Canada")
  # coerce cities lat/long data to an official sf object
  cities <- sf::st_as_sf(
    maps::canada.cities,
    coords = c("long", "lat"),
    crs = 4326
  )
  # A PROJ4 projection designed for Canada
  # http://spatialreference.org/ref/sr-org/7/
  # http://spatialreference.org/ref/sr-org/7/proj4/
  moll_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84
+units=m +no_defs"
  # perform the projections
  canada <- sf::st_transform(canada, moll_proj)
  cities <- sf::st_transform(cities, moll_proj)
  # plot with geom_sf()
  p <- ggplot() +
    geom_sf(data = canada) +
    geom_sf(data = cities, aes(size = pop), color = "red", alpha = 0.3)
  expect_doppelganger_built(ggplotly(p), "sf-axis-ticks")
})
