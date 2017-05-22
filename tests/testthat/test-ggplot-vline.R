context("Vline")

x <- seq(0, 3.5, by = 0.5)
y <- x * 0.95
df <- data.frame(x, y)
gg <- ggplot(df, aes(x, y)) + geom_point()

test_that("second trace be the vline", {
  p <- gg + geom_vline(xintercept = 1.1, colour = "green", size = 3)
  
  L <- save_outputs(p, "vline")
  l <- L$data[[2]]
  
  expect_equivalent(length(L$data), 2)
  expect_equivalent(l$x[1], 1.1)
  expect_true(l$y[1] <= 0)
  expect_true(l$y[2] >= 3.325)
  expect_true(l$mode == "lines")
  expect_true(l$line$color == "rgba(0,255,0,1)")
})

test_that("vector xintercept results in multiple vertical lines", {
  p <- gg + geom_vline(xintercept = 1:2, colour = "blue", size = 3)
  
  L <- save_outputs(p, "vline-multiple")
  expect_equivalent(length(L$data), 2)
  l <- L$data[[2]]
  xs <- unique(l$x)
  ys <- unique(l$y)
  expect_identical(xs, c(1, NA, 2))
  expect_true(min(ys, na.rm = TRUE) <= min(y))
  expect_true(max(ys, na.rm = TRUE) >= max(y))
  expect_true(l$mode == "lines")
  expect_true(l$line$color == "rgba(0,0,255,1)")
})
