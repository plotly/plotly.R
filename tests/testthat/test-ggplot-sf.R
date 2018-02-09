context("geom_sf")

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

test_that("geom_sf() basic polygons.", {
  skip_if_not_installed("sf")
  
  p <- ggplot(nc) + geom_sf()
  
  l <- save_outputs(p, "sf")
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
  l <- save_outputs(p, "sf-geom-collection")
  
  # graticule, point, line, polygon
  expect_length(l$data, 4)
  
  # test data/default for line
  # TODO: test that defaults are correct one geom_sf() becomes stable
  expect_equivalent(l$data[[2]]$x, c(4, 3))
  expect_equivalent(l$data[[2]]$y, c(0, 0))
  expect_equivalent(l$data[[3]]$x, I(1))
  expect_equivalent(l$data[[3]]$y, I(0))
  expect_equivalent(l$data[[4]]$x, c(5.5, 7, 7, 6, 5.5, 5.5))
  expect_equivalent(l$data[[4]]$y, c(0, 0, -.5, -.5, 0, 0))
})

test_that("geom_sf() polygons with fill/text.", {
  skip_if_not_installed("sf")
  
  p <- ggplot(nc) + geom_sf(aes(fill = AREA, text = NAME))
  
  l <- save_outputs(p, "sf-fill-text")
  # one trace for every fillcolor, one for graticule, one for colorbar
  expect_length(l$data, length(unique(nc$AREA)) + 2)
  expect_true(
    all(unlist(lapply(l$data, "[[", "hoverinfo")) %in% c("none", "text"))
  )
  # graticule styling should inherit from panel.grid.major
  expect_equivalent(
    l$data[[1]]$line$color, 
    toRGB(ggplot2::calc_element("panel.grid.major", ggplot2::theme_gray())[["colour"]])
  )
})

test_that("geom_sf() with basic polygons and points.", {
  skip_if_not_installed("sf")
  
  p <- ggplot(nc) +
    geom_sf() +
    annotate("point", x = -80, y = 35, colour = "red", size = 4) +
    theme(panel.grid.major = element_line(colour = "red"))
  
  l <- save_outputs(p, "sf-points")
  # one trace for graticule, one for point, and one polygons
  expect_length(l$data, 3)
  # graticule should be red
  expect_equivalent(l$data[[1]]$line$color, "rgba(255,0,0,1)")
  expect_equivalent(l$data[[2]]$mode, "lines")
  expect_equivalent(l$data[[3]]$mode, "markers")
})

test_that("sf aspect ratio is correct", {
  skip_if_not_installed("sf")
  
  p <- ggplot(nc) + geom_sf() 
  
  l <- save_outputs(p, "sf-aspect")
  expect_equivalent(l$layout$xaxis$scaleanchor, "y")
  expect_equal(l$layout$xaxis$scaleratio, 0.81678435872298)
})


test_that("works with a blank theme", {
  skip_if_not_installed("sf")
  
  p <- ggplot(nc) + geom_sf() + 
    ggthemes::theme_map()
  
  l <- save_outputs(p, "sf-theme-map")
  
  # TODO: perhaps the graticule shouldn't be included at all?
  expect_length(l$data, 2)
  expect_equivalent(l$data[[1]]$line$color, "transparent")
})
