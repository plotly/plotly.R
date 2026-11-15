# Static image export is plotly.core's (save_image(), kaleido()); its own
# tests cover the Kaleido integration.
test_that("save_image() and kaleido() come from plotly.core", {
  expect_identical(save_image, plotly.core::save_image)
  expect_identical(kaleido, plotly.core::kaleido)
})
