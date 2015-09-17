context("Vline")
# Vertical line

x <- seq(0, 3.5, by = 0.5)
y <- x * 0.95
df <- data.frame(x, y)
gg <- ggplot(df, aes(x, y)) + geom_point()

test_that("second trace be the vline", {
  p <- gg + geom_vline(xintercept = 1.1, colour = "green", size = 3)
  
  L <- save_outputs(p, "vline")
  
  expect_equal(length(L$data), 2)
  expect_equal(L$data[[2]]$x[1], 1.1)
  expect_true(L$data[[2]]$y[1] <= 0)
  expect_true(L$data[[2]]$y[2] >= 3.325)
  expect_identical(L$data[[2]]$mode, "lines")
  expect_identical(L$data[[2]]$line$shape, "linear")
  expect_equal(L$data[[2]]$line$width, 6)
  expect_identical(L$data[[2]]$line$color, "rgb(0,255,0)")
})

test_that("vector xintercept results in multiple vertical lines", {
  p <- gg + geom_vline(xintercept = 1:2, colour = "blue", size = 3)
  
  L <- save_outputs(p, "vline-multiple")
  
  expect_equal(length(L$data), 2)
  xs <- unique(L$data[[2]]$x)
  ys <- unique(L$data[[2]]$y)
  expect_identical(xs, c(1, NA, 2))
  expect_true(min(ys, na.rm = TRUE) <= min(y))
  expect_true(max(ys, na.rm = TRUE) >= max(y))
  expect_identical(L$data[[2]]$mode, "lines")
  expect_identical(L$data[[2]]$line$shape, "linear")
  expect_equal(L$data[[2]]$line$width, 6)
  expect_identical(L$data[[2]]$line$color, "rgb(0,0,255)")
})
