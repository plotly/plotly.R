context("linetype")

test_that("6 different automatic lty converted to plotly's 6 types", {
  d <- expand.grid(x=1:6, y=1:6)
  gg <- ggplot() +
    geom_line(aes(x=x, y=y, group=x, linetype=as.factor(x)), data=d)
  expected <-
    c("solid",
      "dash",
      "dot",
      "dashdot",
      "longdash",
      "longdashdot")
  info <- gg2list(gg)
  generated <- sapply(info[1:6], function(L) L$line$dash)
  expect_true(all(generated %in% expected))
  expect_true(all(expected %in% generated))
})
