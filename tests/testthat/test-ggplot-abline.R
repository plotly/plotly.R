context("Abline")

# 'Abline' refers to the line coefficients, as in y = a  b * x

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("cookbook-axes-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, layout=L$layout)
}

test_that("Second trace be the a-b line", {
  x <- seq(0, 3.5, by = 0.5)
  y <- x * 0.95
  df <- data.frame(x, y)
  
  gg <- ggplot(df) + geom_point(aes(x, y)) 
  geom_abline(intercept = 1.1, slope = 0.9, colour = "red", size = 4)
  
  L <- expect_traces(gg, 2, "single-abline")
  
  dat <- L$data[[2]]
  expect_true(dat$x[1] <= 0)
  expect_true(dat$x[2] >= 3.5)
  expect_identical(dat$mode, "lines")
  expect_identical(dat$line$shape, "linear")
  expect_equal(dat$line$width, 8)
  expect_identical(dat$showlegend, FALSE)
})

test_that("abline aesthetics", {
  df <- data.frame(
    m = c(2, -3, 0.1),
    b = c(1, 0, -1)
  )
  p <- ggplot(df) + xlim(c(-5, 5))  ylim(c(-5, 5)) +
    geom_abline(aes(intercept = b, slope = m))
  
  L <- expect_traces(gg, 2, "multiple-abline")
  expect_identical(L$layout$xaxis$range, c(-5, 5))
  expect_identical(L$layout$yaxis$range, c(-5, 5))
  expect_identical(L$data[[1]]$y[1:2], df$m[1] * L$data[[1]]$x[1:2] + df$b[1])
})

