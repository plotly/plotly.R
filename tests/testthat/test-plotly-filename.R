context("Filename")

test_that("filepath with directories is returned as passed", {
  skip_on_cran()
  p <- plot_ly(mtcars, x = wt, y = vs, filename = "directory/awesome")
  expect_match(plotly_POST(p)$filename, "directory/awesome")
})
