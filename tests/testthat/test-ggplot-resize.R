context("ggplotly-resize")

test_that("ggplotly shouldn't populate layout.[width/height] by default", {
  lay <- ggplotly(qplot(1:10))$x$layout
  expect_null(lay$height)
  expect_null(lay$width)
})

# TODO: test the functionality client side!
