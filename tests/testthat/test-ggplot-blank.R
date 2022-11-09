
test_that("geom_blank", {
  skip_if_not_installed("ggplot2", "3.4.0")
  qp <- expect_warning(qplot(), "deprecated")
  l <- ggplotly(qp)$x
  
  expect_length(l$data, 1)
  expect_false(l$data[[1]]$visible)
  
  l <- ggplotly(ggplot())$x
  
  expect_length(l$data, 1)
  expect_false(l$data[[1]]$visible)
  
})
