context("api_download_plot")

test_that("requests made by a user who doesn't exist error a 404", {
  skip_on_cran()
  skip_if_not_master()
  expect_error({
    api_download_plot(0, "klmadslfjdfljdsf")
  }, ".*404.*")
})

test_that("requests made to retrieve a figure that doesn't exist returns a 404", {
  skip_on_cran()
  skip_if_not_master()
  expect_error({
    api_download_plot(18324823, "get_test_user")
  }, ".*404.*")
})

test_that("requests made to retrieve some elses private file errors", {
  skip_on_cran()
  skip_if_not_master()
  expect_error(api_download_plot(1, "get_test_user"))
})

test_that("retrieving a public figure ... works.", {
  skip_on_cran()
  skip_if_not_master()
  fig <- api_download_plot(0, "get_test_user")
  # get the data behind the hash
  p <- plotly_build(fig)$x
  expect_equivalent(p$data[[1]]$x, c("1", "2", "3"))
})

test_that("can add traces to a subplot figure", {
  skip_on_cran()
  skip_if_not_master()
  fig <- api_download_plot(6366, 'chelsea_lyn')
  p <- add_lines(fig, x = c(1, 2, 3), y = c(4, 2, 4))
  expect_equivalent(
    length(plotly_build(fig)$x$data) + 1, 
    length(plotly_build(p)$x$data)
  )
})

test_that("posting a hidden plot returns a secret key", {
  skip_on_cran()
  skip_if_not_master()
  res <- api_create(plot_ly(), sharing = "secret")
  expect_true(res$share_key_enabled)
  expect_true(nchar(res$share_key) > 1)
})

