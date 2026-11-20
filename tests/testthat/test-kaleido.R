# Static image export is plotly.core's (save_image()); its own tests cover it.
test_that("save_image() comes from plotly.core", {
  expect_identical(save_image, plotly.core::save_image)
})

test_that("kaleido() is deprecated in favour of plotly.core's Chrome exporter", {
  skip_on_cran()
  skip_if_not(plotly.core:::chrome_available())
  expect_warning(scope <- kaleido(), "deprecated")
  on.exit(scope$shutdown())
  tmp <- tempfile(fileext = ".svg")
  scope$transform(plot_ly(x = 1:3, y = 1:3), tmp)
  expect_true(file.size(tmp) > 0)
})
