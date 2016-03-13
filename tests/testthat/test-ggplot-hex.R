context("hex")

d <- ggplot(diamonds, aes(carat, price))

test_that("geom_hex", {
  g <- d + geom_hex()
  l <- save_outputs(g, "hex-basic")
  expect_true(length(l$data) > 1)
})


test_that("geom_hex with bins", {
  g <- d + geom_hex(bins = 10)
  l <- save_outputs(g, "hex-bins")
  expect_true(length(l$data) > 1)
})

test_that("geom_hex with binwidth", {
  g <- d + geom_hex(binwidth = c(1, 1000))
  l <- save_outputs(g, "hex-binwidth")
  expect_true(length(l$data) > 1)
})
