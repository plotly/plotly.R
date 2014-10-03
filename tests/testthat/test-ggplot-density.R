context("Probability density")

# Draw a probability density estimation using geom_density
m <- ggplot(movies) + geom_density(aes(rating))
L <- gg2list(m)

test_that("geom_density is translated to a normalized histogram", {
  expect_equal(length(L), 2)
  expect_identical(L[[1]]$type, "histogram")
  expect_true(L[[1]]$autobinx)
  expect_identical(L[[1]]$histnorm, "probability density")
  expect_equal(L[[2]]$layout$bargap, 0)
})
