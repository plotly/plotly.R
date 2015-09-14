context("Filename")

test_that("filepath with directories is returned as passed", {
  p <- print(plot_ly(mtcars, x = wt, y = vs, filename = "directory/awesome"))
  expect_match(p$filename, "directory/awesome")
})
