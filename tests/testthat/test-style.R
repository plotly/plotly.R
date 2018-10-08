context("style/restyle functionality")

p1 <- plot_ly(x = 1:10, y = 1:10, symbol = I(15))
marker1 <- plotly_build(p1)$x$data[[1]]$marker

test_that("Whole update works as expected", {
  p2 <- style(p1, marker = list(color = "red"))
  marker2 <- plotly_build(p2)$x$data[[1]]$marker
  expect_equal(marker2, list(color = "red"))
  
  p3 <- style(p1, marker = list(line = list(color = "red", width = 10)))
  marker3 <- plotly_build(p3)$x$data[[1]]$marker
  expect_equal(marker3, list(line = list(color = "red", width = 10)))
})


test_that("Partial update works as expected", {
  p4 <- style(p1, marker.color = "red")
  marker4 <- plotly_build(p4)$x$data[[1]]$marker
  expect_equal(marker4, modifyList(marker4, list(color = "red")))
  
  p5 <- style(p1, marker.line.color = "red")
  marker5 <- plotly_build(p5)$x$data[[1]]$marker
  expect_equal(marker5, modifyList(marker5, list(line = list(color = "red"))))
})

test_that("Partial update works as expected", {
  p4 <- style(p1, marker.color = "red")
  marker4 <- plotly_build(p4)$x$data[[1]]$marker
  expect_equal(marker4, modifyList(marker4, list(color = "red")))
  
  p5 <- style(p1, marker.line.color = "red")
  marker5 <- plotly_build(p5)$x$data[[1]]$marker
  expect_equal(marker5, modifyList(marker5, list(line = list(color = "red"))))
})


test_that("Partial update works as expected", {
  trace <- list(
    x = 1:5,
    y = 6:10,
    marker = list(line = list(color = "red", width = 20))
  )
  
  trace_new <- trace_replace(trace, c("marker", "line"), list(width = 10))
  trace$marker$line <- list(width = 10)
  expect_equal(trace_new, trace)
  
  trace <- list(
    x = 1:5,
    y = 6:10,
    marker = list(line = list(color = "red", width = 20))
  )
  trace_new <- trace_replace(trace, c("marker", "line", "width"), 10)
  trace$marker$line$width <- 10
  expect_equal(trace_new, trace)
  
  trace <- list(
    x = 1:5,
    y = 6:10
  )
  trace_new <- trace_replace(trace, c("marker", "line", "width"), 10)
  trace$marker$line$width <- 10
  expect_equal(trace_new, trace)
})
