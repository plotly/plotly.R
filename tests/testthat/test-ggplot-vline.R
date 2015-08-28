context("Vline")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("vline-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, layout=L$layout)
}

x <- seq(0, 3.5, by = 0.5)
y <- x * 0.95
df <- data.frame(x, y)
gg <- ggplot(df) + geom_point(aes(x, y))

test_that("second trace be the vline", {
  gg <- gg + 
    geom_vline(xintercept = 1.1, colour = "green", size = 3)
  
  L <- expect_traces(gg, 2, "single")
  dat <- L$data[[2]]
  
  expect_equal(dat$x[1], 1.1)
  expect_true(dat$y[1] <= 0)
  expect_true(dat$y[2] >= 3.325)
  expect_identical(dat$mode, "lines")
  expect_identical(dat$line$shape, "linear")
  expect_equal(dat$line$width, 6)
  expect_identical(dat$line$color, "rgb(0,255,0)")
})

test_that("vector xintercept results in multiple vertical lines", {
  gg <- gg + 
    geom_vline(xintercept = 1:2, colour = "blue", size = 3)
  
  L <- expect_traces(gg, 2, "multiple")
  dat <- L$data[[2]]

  expect_equal(L$data[[2]]$x[1], 1)
  expect_equal(L$data[[3]]$x[1], 2)
  expect_true(L$data[[3]]$y[1] <= 0)
  expect_true(L$data[[3]]$y[2] >= 3.325)
  expect_identical(L$data[[3]]$mode, "lines")
  expect_identical(L$data[[3]]$line$shape, "linear")
  expect_equal(L$data[[3]]$line$width, 6)
  expect_identical(L$data[[3]]$line$color, "rgb(0,0,255)")
})

test_that("vline aesthetics", {
  df <- data.frame(
    m = c(2, -3, 0.1),
    b = c(1, 0, -1)
  )
  
  p <- ggplot(df) + xlim(c(-5, 5)) + ylim(c(-5, 5)) + 
    geom_vline(aes(xintercept = m, linetype = factor(b)))
})
