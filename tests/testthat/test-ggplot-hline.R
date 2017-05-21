context("Hline")
# Horizontal line

x <- seq(0, 3.5, by = 0.5)
y <- x * 0.95
df <- data.frame(x, y)
gg <- ggplot(df) + geom_point(aes(x, y))

test_that("second trace be the hline", {
  p <- gg + geom_hline(yintercept = 1.1, colour = "green", size = 3)
  
  L <- save_outputs(p, "hline")
  expect_equivalent(length(L$data), 2)
  l <- L$data[[2]]
  expect_equivalent(unique(l$y), 1.1)
  expect_true(min(l$x) < min(x))
  expect_true(max(l$x[2]) > max(x))
  expect_identical(l$mode, "lines")
  expect_true(l$line$color == "rgba(0,255,0,1)")
})

test_that("vector yintercept results in multiple horizontal lines", {
  p <- gg + geom_hline(yintercept = 1:3, colour = "red", size = 3)
  
  L <- save_outputs(p, "hline-multiple")
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
  L <- save_outputs(gg, "hline-factor")
  expect_equivalent(length(L$data), 2)  # 1 trace for bar chart, 1 trace for hline
})


test_that("hline/vline/abline split on linetype/colour/size", {
  d <- tibble::tibble(
    x = seq(0, 3.5, by = 0.5), 
    y = x * 0.95
  )
  gg <- ggplot(d, aes(x, y)) +
    geom_vline(xintercept = c(2.5, 3, 3.5), linetype = 1:3) +
    geom_hline(yintercept = c(2.5, 3, 3.5), size = 1:3) +
    geom_abline(slope = -1, intercept = c(2.5, 3, 3.5), colour = 1:3)
  
  l <- plotly_build(gg)$x
  expect_length(l$data, 9)
  
  expect_equivalent(
    vapply(l$data, function(x) x$line$dash, character(1)), 
    lty2dash(c(1:3, rep(1, 6)))
  )
  
  expect_equivalent(
    unique(vapply(l$data, function(x) x$line$color, character(1))),
    c("rgba(0,0,0,1)", "rgba(255,0,0,1)", "rgba(0,205,0,1)")
  )
  
  expect_length(
    unique(vapply(l$data, function(x) x$line$width, numeric(1))), 4
  )
})
