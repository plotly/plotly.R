context("Errorbar")

test_that("geom_errorbar gives errorbars", {

  d <- dplyr::summarise(
    dplyr::group_by_(mtcars, "cyl"), 
    q1 = quantile(mpg, 0.25),
    m = mean(mpg),
    q3 = quantile(mpg, 0.75)
  )

  g <- ggplot(d, aes(x = cyl, y = m)) + geom_line() +
    geom_errorbar(aes(ymin = q1, ymax = q3))

  L <- save_outputs(g, "errorbar")
  
  # 1 trace should have error_y
  idx <- vapply(L$data, function(x) is.null(x$error_y), logical(1))
  expect_true(sum(idx) == 1)
  # right data for errorbar ymax
  expect_equivalent(L$data[!idx][[1]]$error_y$array, d$q3 - d$m)
  expect_equivalent(L$data[!idx][[1]]$error_y$arrayminus, d$m - d$q1)
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
  expect_equivalent(L$data[[1]]$error_y$array, I(0.1))
  expect_equivalent(L$data[[1]]$error_y$arrayminus, I(0.2))
})

# TODO fix and add a test for width of errorbars
