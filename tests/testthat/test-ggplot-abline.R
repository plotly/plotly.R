context("Abline")

# 'Abline' refers to the line coefficients, as in y = a + b * x

test_that("Second trace be the a-b line", {
  x1 <- seq(from=0, to=3.5, by=0.5)
  x2 <- x1 * 0.95
  df <- data.frame("x1"=x1, "x2"=x2)
  
  gg <- ggplot(df) + geom_point(aes(x=x1, y=x2)) +
    geom_abline(intercept=1.1, slope=0.9, colour="red", size=4)
  
  L <- gg2list(gg)

  expect_equal(length(L), 3)
  expect_true(L[[2]]$x[1] <= 0)
  expect_true(L[[2]]$x[2] >= 3.5)
  expect_identical(L[[2]]$mode, "lines")
  expect_identical(L[[2]]$line$shape, "linear")
  expect_equal(L[[2]]$line$width, 8)
  
  expect_identical(L[[1]]$showlegend, FALSE)
  expect_identical(L[[2]]$showlegend, FALSE)

  save_outputs(gg, "abline")
})
