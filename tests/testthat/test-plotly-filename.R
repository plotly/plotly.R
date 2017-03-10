context("Filename")

test_that("filepath with directories is returned as passed", {
  skip_on_cran()
  skip_on_pull_request()
  p <- plot_ly(mtcars, x = ~wt, y = ~vs)
  f <- plotly_POST(p, filename = "directory/awesome")
  expect_match(f$filename, "directory/awesome")
})
