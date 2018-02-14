context("add_sf")

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

test_that("add_sf() is optional", {
  skip_if_not_installed("sf")
  
  p1 <- plotly_build(plot_ly(nc))
  p2 <- plotly_build(plot_ly() %>% add_sf(data = nc))
  
  expect_identical(p1$x$data, p2$x$data)
  
  # all counties drawn in one trace
  expect_length(p1$data, 1)
  
  # 108 polygons
  expect_equal(sum(is.na(p1$x$data[[1]]$x)), 107)
  expect_true(p1$x$data[[1]]$type == "scatter")
  expect_true(p1$x$data[[1]]$mode == "lines")
  expect_true(p1$x$data[[1]]$fill == "toself")
})


test_that("plot_ly() scaleanchor is set", {
  skip_if_not_installed("sf")
  
  p <- plotly_build(plot_ly(nc))
  expect_true(p$x$layout$xaxis$scaleanchor == "y")
  expect_equal(p$x$layout$xaxis$scaleratio, 0.8167844)
})

test_that("plot_geo() lat/lon range is set", {
  skip_if_not_installed("sf")
  
  p <- plotly_build(plot_geo(nc))
  expect_equal(
    p$x$layout$geo$lataxis$range, 
    c(33.85492, 36.61673), 
    tolerance = 1e-6
  )
  expect_equal(
    p$x$layout$geo$lonaxis$range, 
    c(-84.41252, -75.36831), 
    tolerance = 1e-6
  )
})

test_that("plot_mapbox() fitbounds is set", {
  skip_if_not_installed("sf")
  
  p <- plotly_build(plot_mapbox(nc))
  expect_equal(
    p$x$layout$mapbox$`_fitBounds`$bounds, 
    c(-84.41252, 33.85492, -75.36831, 36.61673),
    tolerance = 1e-6
  )
})


test_that("sf defaults can be overriden", {
  p <- plotly_build(plot_mapbox(nc, color = I("red")))
  expect_true(p$x$data[[1]]$type == "scatter")
  expect_true(p$x$data[[1]]$fill == "toself")
  expect_true(p$x$data[[1]]$line$color == toRGB("red"))
  expect_true(p$x$data[[1]]$line$fillcolor == toRGB("red"))
})
