context("Errorbar")

test_that("geom_errorbar gives errorbars", {

  df <- aggregate(mpg~cyl, mtcars, FUN=summary)

  g <- ggplot(df, aes(x=cyl, y=mpg[,'Mean'])) + geom_line() +
    geom_errorbar(aes(ymin=mpg[,'1st Qu.'], ymax=mpg[,'3rd Qu.']))

  L <- gg2list(g)
  
  # right nb. traces (2)
  expect_equal(length(L), 3)
  # trace #2 should be errorbar
  expect_equal(length(L[[2]]$error_y), 2)
  # right data for errorbar ymax
  expect_equal(L[[2]]$error_y$array, c(3.74, 1.26, 1.15))
})
