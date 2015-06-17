context("line")

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
  info <- save_outputs(gg, "linetype-types")
  generated <- sapply(info$data[1:6], function(L) L$line$dash)
  expect_true(all(generated %in% expected))
  expect_true(all(expected %in% generated))
})

test_that("different colored lines become different colored traces", {
  ## http://stackoverflow.com/questions/2564258/plot-2-graphs-in-same-plot-in-r/19039094#19039094

  ## original data in a 'wide' format
  x  <- seq(-2, 2, 0.05)
  y1 <- pnorm(x)
  y2 <- pnorm(x, 1, 1)
  df <- rbind(data.frame(x, variable="y1", value=y1),
              data.frame(x, variable="y2", value=y2))
  ## plot, using the aesthetics argument 'colour'
  gg <- ggplot(data = df, aes(x = x, y = value, colour = variable))+
    geom_line()+
    scale_color_manual(values=c(y1="blue", y2="red"))
  info <- save_outputs(gg, "linetype-colors")
  expect_equal(length(info$data), 2)
  expect_identical(info$data[[1]]$line$color, toRGB("blue"))
  n <- length(x)
  expect_identical(info$data[[1]]$y[1:n], y1)
  expect_identical(info$data[[1]]$x[1:n], x)
  expect_identical(info$data[[2]]$line$color, toRGB("red"))
  expect_identical(info$data[[2]]$y[1:n], y2)
  expect_identical(info$data[[2]]$x[1:n], x)
})
