context("Density2d")

# Draw a 2d density estimation using geom_density2d
m <- ggplot(MASS::geyser) + geom_density2d(aes(x=duration, y=waiting))
L <- gg2list(m)

test_that("geom_density2d is translated to type=histogram2dcontour", {
  expect_equal(length(L), 2)
  expect_identical(L[[1]]$type, "histogram2dcontour")
})

test_that("geom_density2d uses line contours by default", {
  expect_identical(L[[1]]$contours$coloring, "lines")
})
