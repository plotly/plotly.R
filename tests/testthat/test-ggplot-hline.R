context("Hline")
# Horizontal line

x1 <- seq(from=0, to=3.5, by=0.5)
x2 <- x1 * 0.95
df <- data.frame("x1"=x1, "x2"=x2)
gg <- ggplot(df) + geom_point(aes(x=x1, y=x2))

test_that("second trace be the hline", {
  gg <- gg + geom_hline(yintercept=1.1, colour="green", size=3)
  
  L <- gg2list(gg)
  
  expect_equal(length(L$data), 2)
  expect_equal(L$data[[2]]$y[1], 1.1)
  expect_true(L$data[[2]]$x[1] <= 0)
  expect_true(L$data[[2]]$x[2] >= 3.5)
  expect_identical(L$data[[2]]$mode, "lines")
  expect_identical(L$data[[2]]$line$shape, "linear")
  expect_equal(L$data[[2]]$line$width, 6)
  expect_identical(L$data[[2]]$line$color, "rgb(0,255,0)")
  
  save_outputs(gg, "hline")
})

test_that("vector yintercept results in multiple horizontal lines", {
  gg <- gg + geom_hline(yintercept=1:3, colour="red", size=3)
  
  L <- gg2list(gg)
  
  expect_equal(length(L$data), 4)
  expect_equal(L$data[[2]]$y[1], 1)
  expect_equal(L$data[[3]]$y[1], 2)
  expect_equal(L$data[[4]]$y[1], 3)
  expect_true(L$data[[4]]$x[1] <= 0)
  expect_true(L$data[[4]]$x[2] >= 3.325)
  expect_identical(L$data[[3]]$mode, "lines")
  expect_identical(L$data[[3]]$line$shape, "linear")
  expect_equal(L$data[[3]]$line$width, 6)
  expect_identical(L$data[[3]]$line$color, "rgb(255,0,0)")
  
  save_outputs(gg, "hline-multiple")
})

test_that("hline can be drawn over range of factors", {
  df <- data.frame(cond=c("control", "treatment"), result=c(10, 11.5))
  gg <- ggplot(df, aes(x=cond, y=result)) +
    geom_bar(position="dodge", stat="identity") +
    geom_hline(aes(yintercept=12))
  L <- gg2list(gg)
  expect_equal(length(L$data), 2)  # 1 trace for bar chart, 1 trace for hline
  expect_true(all(c("control", "treatment") %in% L$data[[2]]$x))
  
  save_outputs(gg, "hline-factor")
})
