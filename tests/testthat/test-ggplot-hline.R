
x <- seq(0, 3.5, by = 0.5)
y <- x * 0.95
df <- data.frame(x, y)
gg <- ggplot(df) + geom_point(aes(x, y))

test_that("second trace be the hline", {
  skip_if_not_installed("ggplot2", "3.4.0") # linewidth introduced in 3.4.0
  
  p <- gg + geom_hline(yintercept = 1.1, colour = "green", linewidth = 3)
  
  L <- expect_doppelganger_built(p, "hline")
  expect_equivalent(length(L$data), 2)
  l <- L$data[[2]]
  expect_equivalent(unique(l$y), 1.1)
  expect_true(min(l$x) < min(x))
  expect_true(max(l$x[2]) > max(x))
  expect_identical(l$mode, "lines")
  expect_true(l$line$color == "rgba(0,255,0,1)")
})

test_that("vector yintercept results in multiple horizontal lines", {
  skip_if_not_installed("ggplot2", "3.4.0") # linewidth introduced in 3.4.0
  
  p <- gg + geom_hline(yintercept = 1:3, colour = "red", linewidth = 3)
  
  L <- expect_doppelganger_built(p, "hline-multiple")
  expect_equivalent(length(L$data), 2)
  l <- L$data[[2]]
  ys <- l$y
  expect_equivalent(ys, c(1, 1, NA, 2, 2, NA, 3, 3))
  xs <- l$x
  expect_true(min(xs, na.rm = TRUE) < min(x))
  expect_true(max(xs, na.rm = TRUE) > max(x))
  expect_identical(l$mode, "lines")
  expect_true(l$line$color == "rgba(255,0,0,1)")
  
})

test_that("hline can be drawn over range of factors", {
  df <- data.frame(
    cond = c("control", "treatment"), 
    result = c(10, 11.5)
  )
  gg <- ggplot(df, aes(x = cond, y = result)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_hline(aes(yintercept = 12))
  L <- expect_doppelganger_built(gg, "hline-factor")
  expect_equivalent(length(L$data), 2)  # 1 trace for bar chart, 1 trace for hline
})


test_that("hline/vline/abline split on linetype/colour/size", {
  skip_if_not_installed("ggplot2", "3.4.0") # linewidth introduced in 3.4.0
  
  d <- tibble::tibble(
    x = seq(0, 3.5, by = 0.5), 
    y = x * 0.95
  )
  gg <- ggplot(d, aes(x, y)) +
    geom_point() +
    geom_vline(xintercept = c(2.5, 3, 3.5), linetype = 1:3) +
    geom_hline(yintercept = c(2.5, 3, 3.5), linewidth = 1:3) +
    geom_abline(slope = -1, intercept = c(2.5, 3, 3.5), colour = 1:3)
  
  l <- expect_doppelganger_built(gg, "split-hline-vline-abline")
  expect_length(l$data, 10)
  
  expect_equivalent(
    unlist(lapply(l$data, function(x) x$line$dash)), 
    lty2dash(c(1:3, rep(1, 6)))
  )
  
  expect_equivalent(
    unique(unlist(lapply(l$data, function(x) x$line$color))),
    # Default palette colors are changing in R4.0...
    # https://github.com/wch/r-source/commit/58eafa7#diff-038aeefcb87409db883f064615187949R2495
    toRGB(if (getRversion() >= "4.0.0") c("black", "#DF536B", "#61D04F") else c("black", "red", "green3"))
  )
  
  expect_length(
    unique(unlist(lapply(l$data, function(x) x$line$width))), 4
  )
  
  
})


test_that("hline works with coord_flip", {
  
  gg <- ggplot() + 
    geom_point(aes(6, 5)) + 
    geom_hline(yintercept = 5) + 
    coord_flip()
  
  l <- plotly_build(gg)$x
  expect_equivalent(l$data[[2]]$x, c(5, 5))
  expect_equivalent(l$data[[2]]$y, c(5.95, 6.05))
})

test_that("geom_vline/geom_hline does not throw an error with ggplotly when no lines are found", {
  p3 <- ggplot(df) + geom_hline(aes(yintercept = x), data = data.frame(x = 1)[F, , drop = FALSE])
  expect_error(plotly::ggplotly(p3), NA)
})
