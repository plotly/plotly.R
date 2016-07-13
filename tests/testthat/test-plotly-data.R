context("plotly data")

test_that("uniq works as expected", {
  expect_equal(uniq(c("red", "red", NA)), "red")
})

test_that("plotly_data returns empty data frame when none is specified", {
  d <- plotly_data(plot_ly())
  expect_true(is.data.frame(d) && NROW(d) == 0)
})

test_that("plotly_data returns data frame", {
  d <- plotly_data(plot_ly(economics))
  expect_identical(economics, d)
})

test_that("plotly_data preserves groups in data", {
  d <- plotly_data(group_by_(plot_ly(mtcars), c("vs", "am")))
  expect_true(dplyr::groups(d)[[1]] == "vs")
})

