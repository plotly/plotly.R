context("Contour")

volcano3d <- reshape2::melt(volcano)
names(volcano3d) <- c("x", "y", "z")
# Draw a contour plot using geom_contour
gg <- ggplot(volcano3d) + geom_contour(aes(x=x, y=y, z=z))
L <- gg2list(gg)

test_that("geom_contour is translated to type=contour", {
  expect_equal(length(L), 2)
  expect_identical(L[[1]]$type, "contour")
})

test_that("geom_contour uses line contours by default", {
  expect_identical(L[[1]]$contours$coloring, "lines")
})

save_outputs(gg, "contour")
