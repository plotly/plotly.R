context("Filename")

test_that("filename supports names with paths included ", {
  skip_on_cran()
  skip_if_not_master()
  p <- plot_ly(mtcars, x = ~wt, y = ~vs)
  filename <- "directory/awesome"
  # trash the file if it already exists
  file <- api_lookup_file(filename)
  if (is.file(file)) {
    endpt <- sprintf("files/%s/trash", file$fid)
    res <- api(endpt, "POST")
  }
  f <- plotly_POST(p, filename = filename)
  expect_match(f$filename, "awesome")
  expect_true(f$parented)
})
