context("categorical data on the axes")

d <- head(diamonds, 50)

test_that("axis type=category when we plot factors", {
  gg <- qplot(cut, price, data=d)
  info <- gg2list(gg)
  l <- info$layout
  expect_identical(l$xaxis$type, "category")
  expect_identical(l$yaxis$type, "linear")
  
  save_outputs(gg, "bar-factor-category")
})
