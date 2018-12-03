context("device")

test_that("ggplotly doesn't leave a new device open", {
  devList1 <- dev.list()
  p <- ggplotly(ggplot(mtcars) + geom_point(aes(wt, mpg)))
  devList2 <- dev.list()
  expect_true(length(devList1) == length(devList2))
})
