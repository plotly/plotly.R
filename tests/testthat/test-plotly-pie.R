context("pie")

ds <- data.frame(
  labels = c("A", "B", "C"),
  values = c(10, 40, 60)
)

plot_ly(ds, labels = ~labels, values = ~values) %>%
  add_pie()

test_that("No cartesian axes are supplied to a pie chart", {
  p <- plot_ly(ds, labels = ~labels, values = ~values) %>%
    add_pie()
  
  l <- plotly_build(p)$x
  
  expect_true(all(l$data[[1]]$labels == ds$labels))
  expect_true(all(l$data[[1]]$values == ds$values))
  expect_null(l$data[[1]]$textfont)
  expect_null(l$layout$xaxis)
  expect_null(l$layout$yaxis)
  expect_true(l$layout$showlegend)
})



test_that("stroke/span controls slice outline; color/size controls text", {
  p <- plot_ly(ds, labels = ~labels, values = ~values) %>%
    add_pie(color = I("white"), size = I(20), stroke = I("black"), span = I(5)) 
  
  l <- plotly_build(p)$x
  
  expect_true(all(l$data[[1]]$labels == ds$labels))
  expect_true(all(l$data[[1]]$values == ds$values))
  expect_true(l$data[[1]]$textfont$color == toRGB("white"))
  expect_true(l$data[[1]]$textfont$size == 20)
  expect_true(l$data[[1]]$marker$line$color == toRGB("black"))
  expect_true(l$data[[1]]$marker$line$width == 5)
})
