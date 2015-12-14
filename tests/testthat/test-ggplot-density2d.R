context("Density2d")

# Draw a 2d density estimation using geom_density2d
m <- ggplot(MASS::geyser, aes(x=duration, y=waiting)) + 
  geom_point(alpha = 0.4) +
  geom_density2d()
L <- save_outputs(m, "density2d")

test_that("geom_density2d is translated to type=histogram2dcontour", {
  expect_equal(length(L$data), 2)
  expect_identical(L$data[[2]]$type, "histogram2dcontour")
})

test_that("geom_density2d uses line contours by default", {
  expect_identical(L$data[[2]]$contours$coloring, "lines")
})
