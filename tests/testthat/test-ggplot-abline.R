context("Abline")

# 'Abline' refers to the line coefficients, as in y = a  b * x

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("cookbook-axes-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data=has.data, layout=L$layout)
}

test_that("Second trace be the a-b line", {
  x <- seq(0, 3.5, by = 0.5)
  y <- x * 0.95
  df <- data.frame(x, y)
  
  gg <- ggplot(df) + geom_point(aes(x, y, size = x)) +
    geom_abline(intercept = 1.1, slope = 0.9, colour = "red", size = 4)
  
  L <- expect_traces(gg, 2, "single-abline")
  
  dat <- L$data[[2]]
  expect_true(dat$x[1] <= 0)
  expect_true(dat$x[2] >= 3.5)
  expect_identical(dat$mode, "lines")
  expect_identical(dat$showlegend, FALSE)
})

test_that("abline aesthetics", {
  df <- data.frame(
    m = c(2, -3, 0.1),
    b = c(1, 0, -1)
  )
  p <- ggplot(df) + xlim(c(-5, 5)) + ylim(c(-5, 5)) +
    geom_abline(aes(intercept = b, slope = m))
  
  L <- expect_traces(p, 1, "multiple-abline")
  expect_identical(range(L$layout$xaxis$tickvals), c(-5, 5))
  expect_identical(range(L$layout$yaxis$tickvals), c(-5, 5))
})

