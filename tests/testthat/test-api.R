context("api")

test_that("api() returns endpoints", {
  skip_on_cran()
  
  res <- api()
  expect_true(length(res) > 1)
  expect_true(all(c("plots", "grids", "folders") %in% names(res)))
})

test_that("Can search with white-space", {
  skip_on_cran()
  
  res <- api("search?q=overdose drugs")
  expect_true(length(res) > 1)
})


test_that("Downloading plots works", {
  skip_on_cran()
  
  # https://plot.ly/~cpsievert/200
  p <- api_download_plot(200, "cpsievert")
  expect_is(p, "htmlwidget")
  expect_is(p, "plotly")
  
  l <- plotly_build(p)$x
  expect_length(l$data, 1)
  
  # This file is a grid, not a plot https://plot.ly/~cpsievert/14681
  expect_error(
    api_download_plot(14681, "cpsievert"), "grid"
  )
  
})


test_that("Downloading grids works", {
  skip_on_cran()
  
  # https://plot.ly/~cpsievert/200
  p <- api_download_plot(200, "cpsievert")
  expect_is(p, "htmlwidget")
  expect_is(p, "plotly")
  
  l <- plotly_build(p)$x
  expect_length(l$data, 1)
  
  # This file is a grid, not a plot https://plot.ly/~cpsievert/14681
  expect_error(
    api_download_grid(200, "cpsievert"), "plot"
  )
  
})



