context("coord_fixed")

test_that("single-panel fixed coordinates", {
  
  base <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  
  l <- ggplotly(base + coord_equal())$x
  
  expect_equal(l$layout$yaxis$scaleanchor, "x")
  expect_equal(l$layout$yaxis$scaleratio, 1)
  
  l2 <- ggplotly(base + coord_fixed(0.5))$x
  
  expect_equal(l2$layout$yaxis$scaleanchor, "x")
  expect_equal(l2$layout$yaxis$scaleratio, 0.5)
  
})

test_that("multi-panel fixed coordinates", {
  
  base <- ggplot(mtcars, aes(wt, mpg)) + 
    geom_point()
  
  # fixed scales
  p <- base + 
    coord_equal() +
    facet_wrap(~cyl)
  
  l <- ggplotly(p)$x
  
  expect_equal(l$layout$xaxis$scaleanchor, "y")
  expect_equal(l$layout$xaxis$scaleratio, 1)
  expect_equal(l$layout$xaxis2$scaleanchor, "y")
  expect_equal(l$layout$xaxis2$scaleratio, 1)
  expect_equal(l$layout$xaxis3$scaleanchor, "y")
  expect_equal(l$layout$xaxis3$scaleratio, 1)
  
  # free scales
  # NOTE: ggplot2 doesn't even do the "correct" thing here, but we do!
  p <- base + 
    coord_equal() +
    facet_wrap(~cyl, scales = "free")
  
  l <- ggplotly(p)$x
  
  expect_equal(l$layout$yaxis$scaleanchor, "x")
  expect_equal(l$layout$yaxis$scaleratio, 1)
  expect_equal(l$layout$yaxis2$scaleanchor, "x2")
  expect_equal(l$layout$yaxis2$scaleratio, 1)
  expect_equal(l$layout$yaxis3$scaleanchor, "x3")
  expect_equal(l$layout$yaxis3$scaleratio, 1)
  
  # fixed scales (2 rows)
  p <- base + 
    coord_equal() +
    facet_wrap(~cyl, ncol = 2)
  
  l <- ggplotly(p)$x
  
  expect_equal(l$layout$yaxis$scaleanchor, "x")
  expect_equal(l$layout$yaxis$scaleratio, 1)
  expect_equal(l$layout$yaxis2$scaleanchor, "x")
  expect_equal(l$layout$yaxis2$scaleratio, 1)
  expect_equal(l$layout$xaxis$scaleanchor, "y2")
  expect_equal(l$layout$xaxis$scaleratio, 1)
  
  # facet_grid
  p <- base + 
    coord_equal() +
    facet_grid(~cyl)
  
  l <- ggplotly(p)$x
  
  expect_equal(l$layout$xaxis$scaleanchor, "y")
  expect_equal(l$layout$xaxis$scaleratio, 1)
  expect_equal(l$layout$xaxis2$scaleanchor, "y")
  expect_equal(l$layout$xaxis2$scaleratio, 1)
  expect_equal(l$layout$xaxis3$scaleanchor, "y")
  expect_equal(l$layout$xaxis3$scaleratio, 1)
  
})

# TODO: why does this fail on Travis?
# test_that("coord_map", {
#   
#   nz <- map_data("nz")
#   nzmap <- ggplot(nz, aes(x = long, y = lat, group = group)) +
#     geom_polygon(fill = "white", colour = "black") +
#     coord_map()
#   l <- ggplotly(nzmap)$x
#   
#   expect_equal(l$layout$xaxis$scaleanchor, "y")
#   expect_equal(
#     l$layout$xaxis$scaleratio, 0.7023914, tolerance = 0.0001
#   )
# })
