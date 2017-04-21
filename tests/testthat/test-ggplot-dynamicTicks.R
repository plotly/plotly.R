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
  expect_equal(axisActual, axisExpect)
  # trace data reflects the "domain" values
  expect_equal(
    sort(sapply(p$x$data, "[[", "x")), classes
  )
  
  # y-axis is left as expected
  axisActual <- with(
    p$x$layout$yaxis, list(type, tickmode)
  )
  axisExpect <- list("linear", "array")
  expect_equal(axisActual, axisExpect)
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
  expect_equal(axisActual, axisExpect)
  expect_equal(
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
  expect_equal(axisActual, axisExpect)
  expect_equal(
    sort(sapply(p$x$data, "[[", "x")), sort(labs)
  )
  
})
