
x <- seq(0, 3.5, by = 0.5)
y <- x * 0.95
df <- data.frame(x, y)
gg <- ggplot(df, aes(x, y)) + geom_point()

test_that("second trace be the vline", {
  p <- gg + geom_vline(xintercept = 1.1, colour = "green", size = 3)
  
  L <- expect_doppelganger_built(p, "vline")
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
  
  L <- expect_doppelganger_built(p, "vline-multiple")
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



test_that("vline works with coord_flip", {
  
  gg <- ggplot() + 
    geom_point(aes(5, 6)) + 
    geom_vline(xintercept = 5) + 
    coord_flip()
  
  l <- plotly_build(gg)$x
  expect_equivalent(l$data[[2]]$x, c(5.95, 6.05))
  expect_equivalent(l$data[[2]]$y, c(5, 5))
})


# fix for issue #1974 pull request #2252
test_that("geom_vline/geom_hline does not throw an error with ggplotly when no lines are found", {
  set.seed(123)
  x <- seq(0, 10, by = 1)
  # random walk
  y <- cumsum(rnorm(length(x), mean = 0, sd = 1))
  df <- data.frame(x, y)
  df$point_of_interest <- "not_poi"
  df[df$x == 2, ]$point_of_interest <- "poi" # point of interest

  # Case 1: Vertical line by feeding data to it; this allows for programmatically setting many lines at different years
  p1 <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = x),
      data = df[df$point_of_interest == "poi", ],
      colour = "yellow"
    )

  # Test that ggplotly does not throw an error for both cases
  expect_error(plotly::ggplotly(p1), NA) # lines are found no error is thrown

  # Case 2: No lines are found, ggplot2 accepts it and no error is thrown
  p2 <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = x),
      data = df[df$point_of_interest == "something_not_matched", ],
      colour = "yellow"
    )

  # error given without fix:
  # "Error in fix.by(by.y, y) : 'by' must specify a uniquely valid column"
  expect_error(plotly::ggplotly(p2), NA) # no lines are found no error is thrown
})
