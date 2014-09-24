context("categorical data on the axes")

d <- head(diamonds, 50)

test_that("axis type=category when we plot factors", {
  gg <- qplot(cut, price, data=d)
  info <- gg2list(gg)
  l <- info$kwargs$layout
  expect_identical(l$xaxis$type, "category")
  expect_identical(l$yaxis$type, "linear")
})

test_that("Bar charts of type=category show category names", {
  gbar <- ggplot(d, aes(cut, price)) + geom_bar(stat="identity")
  info <- gg2list(gbar)
  
  expect_equal(length(info), 2)  # 1 trace + layout
  expect_identical(info$kwargs$layout$xaxis$type, "category")
  expect_identical(info$kwargs$layout$xaxis$title, "cut")
  expect_identical(info$kwargs$layout$yaxis$type, "linear")
  expect_true(all(c("Fair", "Good", "Very Good", "Premium", "Ideal") %in%
                    info[[1]]$x))
})
