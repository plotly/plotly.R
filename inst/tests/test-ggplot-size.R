context("size")

test_that("size is not a vector if it is not specified",{
  iplot <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))
  L <- gg2list(iplot)
  m <- L[[1]]$marker
  expect_that(m, is_a("list"))
  expect_true(length(m$size) <= 1)
})

test_that("size is a vector if it is specified",{
  iplot <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width, size=Petal.Length))
  L <- gg2list(iplot)
  m <- L[[1]]$marker
  expect_that(m, is_a("list"))
  expect_true(length(m$size) > 1)
})
