context("violin")

gg <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin()

test_that("basic geom_violin works", {
  L <- save_outputs(gg, "violin")
  expect_equal(length(L$data), 1)
  tr <- L$data[[1]]
  expect_identical(tr$type, "scatter")
  expect_true(tr$fill == "tozerox")
  expect_false(tr$showlegend)
  expect_true(all(grepl("density", tr$text)))
  expect_true(tr$hoverinfo == "text")
})


gg2 <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) + geom_violin()

test_that("geom_violin with fill aes works", {
  L <- save_outputs(gg2, "violin-aes")
  expect_equal(length(L$data), 3)
  expect_true(L$layout$showlegend)
  expect_equal(sum(unlist(lapply(L$data, "[[", "showlegend"))), 3)
})


