context("blank")

test_that("geom_blank", {
  l <- ggplotly(qplot())$x
  
  expect_length(l$data, 1)
  expect_false(l$data[[1]]$visible)
  
  l <- ggplotly(ggplot())$x
  
  expect_length(l$data, 1)
  expect_false(l$data[[1]]$visible)
  
})
