
test_that("geom_blank", {
  qp <- expect_warning(qplot(), "deprecated")
  l <- ggplotly(qp)$x
  
  expect_length(l$data, 1)
  expect_false(l$data[[1]]$visible)
  
  l <- ggplotly(ggplot())$x
  
  expect_length(l$data, 1)
  expect_false(l$data[[1]]$visible)
  
})
