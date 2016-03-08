context("Errorbar")

test_that("geom_errorbar gives errorbars", {

  df <- aggregate(mpg~cyl, mtcars, FUN = summary)

  g <- ggplot(df, aes(x = cyl, y = mpg[,'Mean'])) + geom_line() +
    geom_errorbar(aes(ymin = mpg[,'1st Qu.'], ymax = mpg[,'3rd Qu.']))

  L <- save_outputs(g, "errorbar")
  
  # 1 trace should have error_y
  idx <- vapply(L$data, function(x) is.null(x$error_y), logical(1))
  expect_true(sum(idx) == 1)
  # right data for errorbar ymax
  expect_equal(L$data[!idx][[1]]$error_y$array, c(3.74, 1.26, 1.15))
})

df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 3, 4)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p <- ggplot(df, aes(trt, resp, colour = group))
g <- p + geom_errorbar(aes(ymin = lower, ymax = upper))

test_that("geom_errorbar boxes an array of length 1", {
  L <- save_outputs(g, "errorbar-unique-groups")
  expect_true(inherits(L$data[[1]]$error_y$array, "AsIs"))
  expect_true(inherits(L$data[[1]]$error_y$arrayminus, "AsIs"))
})

# TODO fix and add a test for width of errorbars
