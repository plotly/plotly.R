context("lines")

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



test_that("Translates both dates and datetimes (with dynamic ticks) correctly", {
  
  dates <- seq(
    as.Date("2002-01-01"), 
    by = "1 month", 
    length.out = 100
  )
  
  d <- data.frame(
    value = rnorm(100),
    date = dates
  )
  
  p <- ggplot(d, aes(date, value)) + geom_line()
  l <- plotly_build(ggplotly(p, dynamicTicks = TRUE))$x
  
  milliseconds <- as.numeric(d$date) * 86400000
  
  d2 <- data.frame(
    value = rnorm(100),
    date = as.POSIXct(dates)
  )
  
  milliseconds2 <- as.numeric(d2$date) * 1000
  p2 <- ggplot(d2, aes(date, value)) + geom_line()
  l2 <- plotly_build(ggplotly(p2, dynamicTicks = TRUE))$x
  
  # data is all on millisecond level
  expect_equal(milliseconds, milliseconds2)
  expect_equal(milliseconds, l$data[[1]]$x)
  expect_equal(milliseconds, l2$data[[1]]$x)
  
  # same with range
  expect_equal(grDevices::extendrange(milliseconds), l$layout$xaxis$range)
  expect_equal(grDevices::extendrange(milliseconds), l2$layout$xaxis$range)
  
  # since these are dynamic ticks, let plotly.js generate the ticks
  expect_null(l$layout$xaxis$ticktext)
  expect_null(l$layout$xaxis$tickvals)
  expect_null(l2$layout$xaxis$ticktext)
  expect_null(l2$layout$xaxis$tickvals)
  expect_equal(l$layout$xaxis$type, "date")
  expect_equal(l2$layout$xaxis$type, "date")
})
