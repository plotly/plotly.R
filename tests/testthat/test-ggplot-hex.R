d <- ggplot(diamonds, aes(carat, price))

test_that("geom_hex", {
  skip_if_not_installed("hexbin")
  g <- d + geom_hex()
  l <- expect_doppelganger_built(g, "hex-basic")
  expect_true(length(l$data) > 1)
})


test_that("geom_hex with bins", {
  skip_if_not_installed("hexbin")
  g <- d + geom_hex(bins = 10)
  l <- expect_doppelganger_built(g, "hex-bins")
  expect_true(length(l$data) > 1)
})

test_that("geom_hex with binwidth", {
  skip_if_not_installed("hexbin")
  g <- d + geom_hex(binwidth = c(1, 1000))
  l <- expect_doppelganger_built(g, "hex-binwidth")
  expect_true(length(l$data) > 1)
})
