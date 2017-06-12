context("dynamicTicks")

# note: test-ggplot-lines.R has some tests for date dynamicTicks

test_that("Discrete axis maps to categorical type", {
  g <- ggplot(mpg, aes(class, color = class)) + geom_bar()
  p <- ggplotly(g, dynamicTicks = "x")
  
  classes <- getLevels(mpg[["class"]])
  
  axisActual <- with(
    p$x$layout$xaxis, list(type, tickmode, categoryorder, categoryarray)
  )
  axisExpect <- list("category", "auto", "array", classes)
  expect_equivalent(axisActual, axisExpect)
  # trace data reflects the "domain" values
  expect_equivalent(
    sort(sapply(p$x$data, "[[", "x")), classes
  )
  
  # y-axis is left as expected
  axisActual <- with(
    p$x$layout$yaxis, list(type, tickmode)
  )
  axisExpect <- list("linear", "array")
  expect_equivalent(axisActual, axisExpect)
})

test_that("Categorical axis reflects custom scale mapping", {
  
  lims <- c("2seater", "suv")
  
  g <- ggplot(mpg, aes(class, color = class)) + 
    geom_bar() +
    scale_x_discrete(limits = lims)
  p <- ggplotly(g, dynamicTicks = "x")
  
  axisActual <- with(
    p$x$layout$xaxis, list(type, tickmode, categoryorder, categoryarray)
  )
  axisExpect <- list("category", "auto", "array", lims)
  expect_equivalent(axisActual, axisExpect)
  expect_equivalent(
    sort(sapply(p$x$data, "[[", "x")), sort(lims)
  )
  
  labs <- c("small", "large")
  g <- ggplot(mpg, aes(class, color = class)) + 
    geom_bar() +
    scale_x_discrete(limits = lims, labels = labs)
  p <- ggplotly(g, dynamicTicks = "x")
  
  axisActual <- with(
    p$x$layout$xaxis, list(type, tickmode, categoryorder, categoryarray)
  )
  axisExpect <- list("category", "auto", "array", labs)
  expect_equivalent(axisActual, axisExpect)
  expect_equivalent(
    sort(sapply(p$x$data, "[[", "x")), sort(labs)
  )
  
})

test_that("Time axis inverse transforms correctly", {
  
  d <- data.frame(
    x = seq(Sys.Date(), Sys.Date() + 9, length.out = 10), 
    y = rnorm(10)
  )
  
  l <- ggplotly(ggplot(d, aes(x, y)) + geom_line(), dynamicTicks = TRUE)$x
  
  expect_length(l$data, 1)
  expect_equivalent(l$layout$xaxis$type, "date")
  expect_equivalent(l$layout$xaxis$tickmode, "auto")
  expect_is(l$layout$xaxis$range, "Date")
  expect_equivalent(l$data[[1]][["x"]], d$x)
  
  d2 <- data.frame(
    x = seq(Sys.time(), Sys.time() + 9000, length.out = 10), 
    y = rnorm(10)
  )
  
  l2 <- ggplotly(ggplot(d2, aes(x, y)) + geom_line(), dynamicTicks = TRUE)$x
  
  expect_length(l2$data, 1)
  expect_equivalent(l2$layout$xaxis$type, "date")
  expect_equivalent(l2$layout$xaxis$tickmode, "auto")
  expect_is(l2$layout$xaxis$range, "POSIXt")
  expect_equivalent(l2$data[[1]][["x"]], d2$x)
  
})


test_that("Inverse maps colorbar data", {
  
  p <- ggplot(mpg, aes(hwy, manufacturer)) + 
    stat_bin2d(aes(fill = ..density..), binwidth = c(3,1))
  
  l <- ggplotly(p, dynamicTicks = TRUE)$x
  
  expect_length(l$data, 2)
  expect_true(l$data[[2]]$y %in% unique(mpg$manufacturer))
  
})

