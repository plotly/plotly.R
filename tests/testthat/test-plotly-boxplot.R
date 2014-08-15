context("Boxplot")

test_that("geom_boxplot gives a boxplot", {
  gg <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
  
  L <- gg2list(gg)

  ## right nb. traces
  expect_equal(length(L), 4)
  ## right type for 1st trace
  expect_identical(L[[1]]$type, "box")
  ## right data for 1st trace
  expect_identical(sort(L[[1]]$y),
                   sort(mtcars$mpg[mtcars$cyl == 4]))
})
