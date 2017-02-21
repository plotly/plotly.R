context("geom_sf")

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
p <- ggplot(nc) + geom_sf()

test_that("geom_sf() basic polygons.", {
  skip_if_not_installed("sf")
  
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

p <- ggplot(nc) + geom_sf(aes(fill = AREA, text = NAME))

test_that("geom_sf() polygons with fill/text.", {
  skip_if_not_installed("sf")
  
  l <- save_outputs(p, "sf-fill-text")
  # one trace for graticule, one for colorbar, and one for each row
  expect_length(l$data, nrow(nc) + 2)
  expect_true(
    all(unlist(lapply(l$data, "[[", "hoverinfo")) %in% c("none", "text"))
  )
  unlist(lapply(l$data, "[[", "text"))
})

p <- ggplot(nc) +
  geom_sf() +
  annotate("point", x = -80, y = 35, colour = "red", size = 4) +
  theme(panel.grid.major = element_line(colour = "red"))

test_that("geom_sf() with basic polygons and points.", {
  skip_if_not_installed("sf")
  
  l <- save_outputs(p, "sf-points")
  # one trace for graticule, one for point, and one polygons
  expect_length(l$data, 3)
  # graticule should be red
  expect_equal(l$data[[1]]$line$color, "rgba(255,0,0,1)")
  expect_equal(l$data[[2]]$mode, "lines")
  expect_equal(l$data[[3]]$mode, "markers")
})
