context("do not generate")

test_that("size is not generated if it is not specified",{
  iplot <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))
  L <- gg2list(iplot)
  m <- L[[1]]$marker
  expect_that(m, is_a("list"))
  expect_that(m$size, is_a("NULL"))
})
