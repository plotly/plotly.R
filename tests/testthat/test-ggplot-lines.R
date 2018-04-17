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
  expect_equivalent(length(info$data), 2)
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

  d2 <- data.frame(
    value = rnorm(100),
    date = as.POSIXct(dates)
  )
  
  p2 <- ggplot(d2, aes(date, value)) + geom_line()
  l2 <- plotly_build(ggplotly(p2, dynamicTicks = TRUE))$x
  
  # since these are dynamic ticks, let plotly.js generate the ticks
  axisType <- with(l$layout$xaxis, list(type, tickmode, autorange))
  expect_equivalent(axisType, list("date", "auto", TRUE))
  axisType2 <- with(l2$layout$xaxis, list(type, tickmode, autorange))
  expect_equivalent(axisType2, list("date", "auto", TRUE))
  
  # range and data have been reverse transformed
  expect_is(l$layout$xaxis$range, "Date")
  expect_is(l$data[[1]]$x, "Date")
  expect_is(l2$layout$xaxis$range, "POSIXct")
  expect_is(l2$data[[1]]$x, "POSIXct")
  
  # check the hovertext
  dates1 <- sapply(strsplit(l$data[[1]]$text, br()), "[[", 1)
  dates2 <- sapply(strsplit(l2$data[[1]]$text, br()), "[[", 1)
  expect_equivalent(paste("date:", d$date), dates1)
  expect_equivalent(paste("date:", d2$date), dates2)
})

test_that("geom_linerange() without a y aesthetic translates to a path", {
  d <- data.frame(
    x = 1:5, 
    ymax = 1:5,
    ymin = 0
  )
  
  p <- ggplot(d, aes(x, ymax = ymax, ymin = ymin)) +  
    geom_linerange()
  
  l <- plotly_build(p)$x
  
  expect_length(l$data, 1)
  expect_equivalent(l$data[[1]]$type, "scatter")
  expect_equivalent(
    l$data[[1]]$x,
    c(1, 1, NA, 2, 2, NA, 3, 3, NA, 4, 4, NA, 5, 5)
  )
  expect_equivalent(
    l$data[[1]]$y,
    c(0, 1, NA, 0, 2, NA, 0, 3, NA, 0, 4, NA, 0, 5)
  )
  expect_equivalent(
    unlist(l$data[[1]]$text),
    c(
      'x: 1<br />ymax: 1<br />ymin: 0', 'x: 1<br />ymax: 1<br />ymin: 0', NA, 
      'x: 2<br />ymax: 2<br />ymin: 0', 'x: 2<br />ymax: 2<br />ymin: 0', NA, 
      'x: 3<br />ymax: 3<br />ymin: 0', 'x: 3<br />ymax: 3<br />ymin: 0', NA, 
      'x: 4<br />ymax: 4<br />ymin: 0', 'x: 4<br />ymax: 4<br />ymin: 0', NA, 
      'x: 5<br />ymax: 5<br />ymin: 0', 'x: 5<br />ymax: 5<br />ymin: 0'
    )
  )
  
})
