context("Errorbar")

test_that("geom_errorbar gives errorbars", {

  df <- aggregate(mpg~cyl, mtcars, FUN=summary)

  g <- ggplot(df, aes(x=cyl, y=mpg[,'Mean'])) + geom_line() +
    geom_errorbar(aes(ymin=mpg[,'1st Qu.'], ymax=mpg[,'3rd Qu.']))

  L <- gg2list(g)

  # right nb. traces (1)
  expect_equal(length(L), 2)
  # trace #1 should be errorbar
  expect_more_than(length(L[[1]]$error_y), 1)
  # right data for errorbar ymax
  expect_equal(L[[1]]$error_y$array, c(3.74, 1.26, 1.15))

  save_outputs(g, "errorbar")
})

test_that("geom_errorbarh gives horizontal errorbars", {

  df <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    se = c(0.1, 0.3, 0.3, 0.4)
  )
  g <- ggplot(df, aes(resp, trt, colour=group)) + geom_point()
  # Define the limits of the horizontal errorbars
  g <- g + geom_errorbarh(aes(xmax = resp + se, xmin = resp - se))

  L <- gg2list(g)

  # Expect 2 traces
  expect_equal(length(L), 3)
  # Expect scatter plot and its error bars to have the same color
  expect_identical(L[[1]]$marker$color, L[[1]]$error_x$color)
  expect_identical(L[[2]]$marker$color, L[[2]]$error_x$color)
  # Expect given errorbar values
  expect_equal(L[[1]]$error_x$array, c(0.1, 0.3))
  expect_equal(L[[1]]$error_x$symmetric, TRUE)

  save_outputs(g, "errorbar-horizontal")
})
