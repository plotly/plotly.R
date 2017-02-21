context("device")

test_that("ggplotly doesn't leave a new device open", {
  devList1 <- dev.list()
  p <- ggplotly(qplot(1:10))
  devList2 <- dev.list()
  expect_true(length(devList1) == length(devList2))
})
