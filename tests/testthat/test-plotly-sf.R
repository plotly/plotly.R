context("add_sf")

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

has_mapbox <- function() {
  !is.null(tryNULL(mapbox_token()))
}

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
  skip_if_not(has_mapbox())
  
  p <- plotly_build(plot_mapbox(nc))
  expect_equal(
    p$x$layout$mapbox$`_fitBounds`$bounds, 
    c(-84.41252, 33.85492, -75.36831, 36.61673),
    tolerance = 1e-5
  )
})


test_that("sf defaults can be overriden", {
  skip_if_not_installed("sf")
  skip_if_not(has_mapbox())
  
  # when applied to fillcolor, alpha defaults to 0.5
  p <- plotly_build(plot_mapbox(nc, color = I("red")))
  expect_true(p$x$data[[1]]$type == "scattermapbox")
  expect_true(p$x$data[[1]]$fill == "toself")
  expect_true(p$x$data[[1]]$line$color == toRGB("red"))
  expect_true(p$x$data[[1]]$fillcolor == toRGB("red", 0.5))
  
  p <- plotly_build(plot_mapbox(nc, color = I("red"), alpha = 0.8))
  expect_true(p$x$data[[1]]$line$color == toRGB("red"))
  expect_true(p$x$data[[1]]$fillcolor == toRGB("red", 0.8))
  
  p <- plotly_build(plot_mapbox(nc, color = I("red"), stroke = I("black"), alpha_stroke = 0.2))
  expect_true(p$x$data[[1]]$line$color == toRGB("black", 0.2))
  expect_true(p$x$data[[1]]$fillcolor == toRGB("red", 0.5))
})

test_that("Can plot sfc with a missing crs", {
  skip_if_not_installed("sf")
  
  p <- plotly_build(plot_geo(storms, name = "Storms"))
  expect_true(p$x$data[[1]]$type == "scattergeo")
  expect_true(p$x$data[[1]]$mode == "lines")
})


test_that("plot_ly() defaults to blank axes", {
  skip_if_not_installed("sf")
  skip_if_not_installed("maptools")
  skip_if_not_installed("rgeos")
  
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
  
  expect_true(xaxis$ticks == "")
  expect_true(yaxis$ticks == "")
})

test_that("discrete color informs fillcolor", {
  skip_if_not_installed("sf")
  skip_if_not(has_mapbox())
  
  res <- unique(res_mn$INDRESNAME)
  cols <- viridisLite::magma(length(res))
  
  p <- plot_mapbox(res_mn, color = ~INDRESNAME, colors = cols) %>%
    plotly_build()
  
  d <- p$x$data
  expect_length(d, length(res))
  
  fillcolors <- sapply(d, "[[", "fillcolor")
  expect_identical(fillcolors, toRGB(cols, 0.5))
  
  # 'stroke' should inherit from fillcolor
  linecolors <- sapply(d, function(tr) tr$line$color)
  expect_identical(linecolors, toRGB(cols))
})


test_that("discrete color informs fillcolor", {
  skip_if_not_installed("sf")
  skip_if_not(has_mapbox())
  
  res <- unique(res_mn$INDRESNAME)
  cols <- viridisLite::magma(length(res))
  
  p <- plot_mapbox(res_mn, color = ~INDRESNAME, colors = cols) %>%
    plotly_build()
  
  d <- p$x$data
  expect_length(d, length(res))
  
  # alpha defaults to 0.5 when applied to fillcolor
  fillcolors <- sapply(d, "[[", "fillcolor")
  expect_identical(fillcolors, toRGB(cols, 0.5))
  
  # 'stroke' inherits from 'color'
  linecolors <- sapply(d, function(tr) tr$line$color)
  expect_identical(linecolors, toRGB(cols))
  
  # make sure we can set alpha/stroke
  p <- plot_mapbox(res_mn, color = ~INDRESNAME, colors = cols, alpha = 1, stroke = I("black")) %>%
    plotly_build()
  
  d <- p$x$data
  expect_length(d, length(res))
  
  fillcolors <- sapply(d, "[[", "fillcolor")
  expect_identical(fillcolors, toRGB(cols))
  
  linecolors <- sapply(d, function(tr) tr$line$color)
  expect_match(linecolors, "rgba(0,0,0,1)", fixed = TRUE)
})


test_that("numeric color informs fillcolor", {
  skip_if_not_installed("sf")
  skip_if_not(has_mapbox())
  
  p <- plot_mapbox(res_mn, color = ~AREA)
  expect_warning(plotly_build(p), "Only one fillcolor per trace allowed")
  
  d <- plotly_build(p)$x$data
  expect_true(d[[1]]$mode == "lines")
  
  
  res <- unique(res_mn$INDRESNAME)
  cols <- viridisLite::magma(length(res))
  p <- plot_mapbox(res_mn, split = ~INDRESNAME, color = ~AREA, colors = cols) %>%
    plotly_build()
  
  d <- p$x$data
  expect_length(d, length(res) + 1)
  
  # alpha defaults to 0.5 when applied to fillcolor
  area <- unique(res_mn$AREA)
  fillcolors <- unlist(lapply(d, "[[", "fillcolor"))
  areacolors <- scales::col_numeric(cols, range(area))(area)
  expect_identical(sort(fillcolors), sort(toRGB(areacolors, 0.5)))
  
  # 'stroke' should inherit from fillcolor
  # TODO: should strokes inherit from colors?
  linecolors <- unlist(lapply(d, function(tr) tr$line$color))
  expect_identical(sort(linecolors), sort(toRGB(areacolors)))
  
  # can set alpha_stroke
  p <- plot_mapbox(res_mn, split = ~INDRESNAME, color = ~AREA, colors = cols, alpha_stroke = 0) %>%
    plotly_build()
  d <- p$x$data
  expect_length(d, length(res) + 1)
  areas <- unique(res_mn$AREA)
  pal <- scales::col_numeric(cols, domain = range(areas))(areas)
  fillcolors <- unlist(lapply(d, function(tr) tr$fillcolor))
  expect_identical(sort(fillcolors), sort(toRGB(pal, 0.5)))
  linecolors <- unlist(lapply(d, function(tr) tr$line$color))
  expect_identical(sort(linecolors), sort(toRGB(pal, 0)))
})


test_that("sizing constants", {
  skip_if_not_installed("sf")
  skip_if_not(has_mapbox())
  
  # span controls 'stroke-size'
  p <- plot_mapbox(res_mn, span = I(5)) %>% plotly_build()
  d <- p$x$data
  expect_length(d, 1)
  expect_true(d[[1]]$line$width == 5)
  
  # size controls marker-size
  mn_pts <- sf::st_centroid(res_mn)
  p <- plot_mapbox(mn_pts, size = I(30)) %>% plotly_build()
  d <- p$x$data
  expect_length(d, 1)
  expect_true(d[[1]]$marker$size == 30)
  expect_true(d[[1]]$marker$sizemode == "area")
  
  # span controls marker.line.width
  p <- plot_ly(mn_pts, size = I(30), span = I(10), stroke = I("black")) %>% plotly_build()
  d <- p$x$data
  expect_length(d, 1)
  expect_true(d[[1]]$marker$size == 30)
  expect_true(d[[1]]$marker$sizemode == "area")
  expect_true(d[[1]]$marker$line$width == 10)
  expect_true(d[[1]]$marker$line$color == toRGB("black"))
  
  # size controls error_x.width
  p <- plot_ly(mn_pts, size = I(20), error_x = list(value = 5)) %>% plotly_build()
  d <- p$x$data
  expect_length(d, 1)
  expect_true(d[[1]]$marker$size == 20)
  expect_true(d[[1]]$marker$sizemode == "area")
  expect_true(d[[1]]$error_x$value == 5)
  expect_true(d[[1]]$error_x$width == 20)

  
  # size controls textfont.size
  p <- plot_ly(mn_pts, size = I(20), text = "MN rocks", mode = "text") %>% plotly_build()
  d <- p$x$data
  expect_length(d, 1)
  expect_true(d[[1]]$mode == "text")
  expect_true(d[[1]]$textfont$size == 20)
})


test_that("size mappings", {
  skip_if_not_installed("sf")
  skip_if_not(has_mapbox())
  
  expect_warning(
    plotly_build(plot_mapbox(res_mn, span = ~PERIMETER)), 
    "`line.width` does not currently support multiple values"
  )
  
  # TODO: should a `size` mapping yield the same result in this case?
  res <- unique(res_mn$INDRESNAME)
  p <- plot_mapbox(res_mn, span = ~PERIMETER, split = ~INDRESNAME) %>% plotly_build()
  d <- p$x$data
  expect_length(d, length(res))
  perimeters <- unique(res_mn$PERIMETER)
  widths <- sapply(d, function(tr) tr$line$width)
  expect_equal(sort(widths), sort(scales::rescale(perimeters, to = c(1, 20))))
  
  
  mn_pts <- sf::st_centroid(res_mn)
  p <- plot_ly(mn_pts, size = ~AREA, span = I(10), stroke = I("black")) %>% plotly_build()
  d <- p$x$data
  expect_length(d, 1)
  areas <- unique(res_mn$AREA)
  sizes <- unlist(lapply(d, function(tr) tr$marker$size))
  expect_equal(sort(sizes), sort(scales::rescale(areas, to = c(10, 100))))
  
  p <- plot_ly(mn_pts, size = I(10), span = ~PERIMETER, stroke = I("black")) %>% plotly_build()
  d <- p$x$data
  expect_length(d, 1)
  perimeters <- unique(res_mn$PERIMETER)
  widths <- unlist(lapply(d, function(tr) tr$marker$line$width))
  expect_equal(sort(widths), sort(scales::rescale(perimeters, to = c(1, 20))))
})


test_that("altogether now", {
  skip_if_not_installed("sf")
  skip_if_not(has_mapbox())
  
  s <- subplot(plot_ly(nc), plot_geo(nc), plot_mapbox(nc), nrows = 3) %>% plotly_build()
  d <- s$x$data
  expect_length(d, 3)
  linecolors <- sapply(d, function(tr) tr$line$color)
  fillcolors <- sapply(d, function(tr) tr$fillcolor)
  expect_equal(linecolors, toRGB(colorway()[1:3]))
  # should be 0.5, but close enough
  expect_equal(fillcolors, toRGB(colorway()[1:3], 0.498))

  
  # specify a colorway
  cols <- c("red", "blue", "green")
  s <- subplot(
    layout(plot_ly(nc), colorway = cols), 
    plot_geo(nc), plot_mapbox(nc), nrows = 3
  ) 
  d <- s$x$data
  expect_length(d, 3)
  linecolors <- sapply(d, function(tr) tr$line$color)
  fillcolors <- sapply(d, function(tr) tr$fillcolor)
  expect_equal(linecolors, toRGB(cols))
  # should be 0.5, but close enough
  expect_equal(fillcolors, toRGB(cols, 0.498))
  
  # TODO: this should also work
  # layout(s, colorway = c("red", "blue", "green"))
  
})



test_that("color and stroke scales can be set independently", {
  skip_if_not_installed("sf")
  skip_if_not(has_mapbox())
  
  n <- length(unique(res_mn$INDRESNAME))
  p <- plot_mapbox(res_mn, split = ~INDRESNAME, color = ~AREA, stroke = ~PERIMETER, span = I(2)) %>%
    plotly_build()
  
  # two colorbars
  d <- p$x$data
  expect_true(length(d) == n + 2)
  
  colorbars <- d[vapply(d, is.colorbar, logical(1))]
  
  expect_true(colorbars[[1]]$marker$colorbar$title == "AREA")
  expect_true(colorbars[[2]]$marker$colorbar$title == "PERIMETER")
  expect_true(all(colorbars[[1]]$marker$color == range(res_mn$AREA)))
  expect_true(all(colorbars[[2]]$marker$color == range(res_mn$PERIMETER)))
  
})
