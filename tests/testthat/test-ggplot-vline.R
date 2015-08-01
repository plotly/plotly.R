context("Vline")
# Vertical line

x1 <- seq(from=0, to=3.5, by=0.5)
x2 <- x1 * 0.95
df <- data.frame("x1"=x1, "x2"=x2)
gg <- ggplot(df) + geom_point(aes(x=x1, y=x2))

test_that("second trace be the vline", {
  gg <- gg + geom_vline(xintercept=1.1, colour="green", size=3)
  
  L <- save_outputs(gg, "vline")
  
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
  gg <- gg + geom_vline(xintercept=1:2, colour="blue", size=3)
  
  L <- save_outputs(gg, "vline-multiple")
  
  expect_equal(length(L$data), 3)
  expect_equal(L$data[[2]]$x[1], 1)
  expect_equal(L$data[[3]]$x[1], 2)
  expect_true(L$data[[3]]$y[1] <= 0)
  expect_true(L$data[[3]]$y[2] >= 3.325)
  expect_identical(L$data[[3]]$mode, "lines")
  expect_identical(L$data[[3]]$line$shape, "linear")
  expect_equal(L$data[[3]]$line$width, 6)
  expect_identical(L$data[[3]]$line$color, "rgb(0,0,255)")
})
