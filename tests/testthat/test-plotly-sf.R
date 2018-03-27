context("add_sf")

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

test_that("add_sf() is optional", {
  skip_if_not_installed("sf")
  
  p1 <- plotly_build(plot_ly(nc))
  p2 <- plotly_build(plot_ly() %>% add_sf(data = nc))
  
  expect_identical(p1$x$data, p2$x$data)
  
  # all counties drawn in one trace
  expect_length(p1$x$data, 1)
  
  # 108 polygons
  expect_equal(sum(is.na(p1$x$data[[1]]$x)), 107)
  expect_true(p1$x$data[[1]]$type == "scatter")
  expect_true(p1$x$data[[1]]$mode == "lines")
  expect_true(p1$x$data[[1]]$fill == "toself")
  
  # scaleanchor is set
  expect_true(p1$x$layout$xaxis$scaleanchor == "y")
  expect_equal(p1$x$layout$xaxis$scaleratio, 0.8167844, tolerance = 1e-6)
})


test_that("plot_geo() lat/lon range is set", {
  skip_if_not_installed("sf")
  
  p <- plotly_build(plot_geo(nc))
  expect_equal(
    p$x$layout$geo$lataxis$range, 
    c(33.85492, 36.61673), 
    tolerance = 1e-5
  )
  expect_equal(
    p$x$layout$geo$lonaxis$range, 
    c(-84.41252, -75.36831), 
    tolerance = 1e-5
  )
})

test_that("plot_mapbox() fitbounds is set", {
  skip_if_not_installed("sf")
  
  p <- plotly_build(plot_mapbox(nc))
  expect_equal(
    p$x$layout$mapbox$`_fitBounds`$bounds, 
    c(-84.41252, 33.85492, -75.36831, 36.61673),
    tolerance = 1e-5
  )
})


test_that("sf defaults can be overriden", {
  skip_if_not_installed("sf")
  
  p <- plotly_build(plot_mapbox(nc, color = I("red")))
  expect_true(p$x$data[[1]]$type == "scattermapbox")
  expect_true(p$x$data[[1]]$fill == "toself")
  expect_true(p$x$data[[1]]$line$color == toRGB("red"))
  expect_true(p$x$data[[1]]$line$fillcolor == toRGB("red"))
})

test_that("Can plot sfc with a missing crs", {
  skip_if_not_installed("sf")
  
  p <- plotly_build(plot_geo(storms, name = "Storms"))
  expect_true(p$x$data[[1]]$type == "scattergeo")
  expect_true(p$x$data[[1]]$mode == "lines")
})


test_that("plot_ly() defaults to blank axes", {
  skip_if_not_installed("sf")
  
  m <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
  
  p <- plot_ly() %>%
    add_sf(data = m, color = I("black"), fillcolor = "transparent", hoverinfo = "none") %>%
    layout(xaxis = list(title = "just a test")) %>%
    plotly_build()
  
  xaxis <- p$x$layout$xaxis
  yaxis <- p$x$layout$yaxis
  
  expect_false(xaxis$showgrid)
  expect_false(yaxis$showgrid)
  
  expect_false(xaxis$showticklabels)
  expect_false(yaxis$showticklabels)
  
  expect_false(xaxis$zeroline)
  expect_false(yaxis$zeroline)
  
  expect_true(xaxis$title == "just a test")
  expect_null(yaxis$title)
  
  expect_true(yaxis$ticks == "")
  expect_true(yaxis$ticks == "")
})

test_that("discrete color informs fillcolor", {
  res <- unique(res_mn$INDRESNAME)
  cols <- viridisLite::magma(length(res))
  
  p <- plot_mapbox(res_mn, color = ~INDRESNAME, colors = cols) %>%
    plotly_build()
  
  d <- p$x$data
  expect_length(d, length(res))
  
  fillcolors <- sapply(d, "[[", "fillcolor")
  expect_identical(fillcolors, toRGB(cols))
  
  # 'stroke' should inherit from fillcolor
  linecolors <- sapply(d, function(tr) tr$line$color)
  expect_identical(linecolors, toRGB(cols))
})


test_that("discrete color informs fillcolor", {
  res <- unique(res_mn$INDRESNAME)
  cols <- viridisLite::magma(length(res))
  
  p <- plot_mapbox(res_mn, color = ~INDRESNAME, colors = cols) %>%
    plotly_build()
  
  d <- p$x$data
  expect_length(d, length(res))
  
  fillcolors <- sapply(d, "[[", "fillcolor")
  expect_identical(fillcolors, toRGB(cols))
  
  # 'stroke' should inherit from fillcolor
  linecolors <- sapply(d, function(tr) tr$line$color)
  expect_identical(linecolors, toRGB(cols))
})


test_that("numeric color informs fillcolor", {
  res <- unique(res_mn$INDRESNAME)
  cols <- viridisLite::magma(length(res))
  p <- plot_mapbox(mn_res, split = ~INDRESNAME, color = ~AREA, colors = cols, showlegend = FALSE, line = list(color = "black")) %>%
    plotly_build()
  
  d <- p$x$data
  expect_length(d, length(res) + 1)
  
  area <- unique(res_mn$AREA)
  fillcolors <- unlist(lapply(d, "[[", "fillcolor"))
  expect_identical(sort(fillcolors), sort(scales::col_numeric(cols, range(area))(area)))
  
  # 'stroke' should inherit from fillcolor
  linecolors <- unlist(lapply(d, function(tr) tr$line$color))
  expect_identical(linecolors, rep("black", length(res)))
})


