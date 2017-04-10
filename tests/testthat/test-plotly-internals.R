context("internals")

# test some assumptions we make about internal functions

test_that("remove_class() can remove 'AsIs' class", {
  x <- 1:10
  class(x) <- c("x", "y")
  x <- remove_class(I(x), "AsIs")
  expect_equal(class(x), c("x", "y"))
})
