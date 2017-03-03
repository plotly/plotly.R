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
  expect_null(l$layout$xaxis)
  expect_null(l$layout$yaxis)
})
