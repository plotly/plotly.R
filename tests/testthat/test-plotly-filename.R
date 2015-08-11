context("Filename")

test_that("filepath with directories is returned as passed", {
  p <- print(plot_ly(mtcars, x = wt, y = vs, filename = "directory/awesome"))
  # why is the directory name replicated in the response?
  expect_identical(p$filename, "directorydirectory/awesome")
})
