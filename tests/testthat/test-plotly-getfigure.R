context("get_figure")

test_that("requests made by a user who doesn't exist error a 404", {
  skip_on_cran()
  expect_error({
    get_figure("klmadslfjdfljdsf", 0)
  }, ".*404.*")
})

test_that("requests made to retrieve a figure that doesn't exist returns a 404", {
  skip_on_cran()
  expect_error({
    get_figure("get_test_user", 18324823)
  }, ".*404.*")
})

test_that("requests made to retrieve some elses private file errors a 403", {
  skip_on_cran()
  expect_error({
    get_figure("get_test_user", 1)
  }, ".*403.*")
})

test_that("retrieving a public figure ... works.", {
  skip_on_cran()
  fig <- get_figure("get_test_user", 0)
  # get the data behind the hash
  p <- plotly_build(fig)
  expect_equivalent(p$data[[1]]$x, c("1", "2", "3"))
})

test_that("can add traces to a subplot figure", {
  skip_on_cran()
  fig <- get_figure('chelsea_lyn', 6366)
  p <- add_trace(fig, x=c(1, 2, 3), y=c(4, 2, 4))
  l <- plotly_build(p)
  expect_equivalent(length(plotly_build(fig)$data) + 1, length(l$data))
})

test_that("posting a hidden plot returns a secret key", {
  skip_on_cran()
  res <- plotly_POST(plot_ly(), sharing = "secret")
  key <- strsplit(res$url, "=")[[1]][2]
  expect_true(nchar(key) > 1)
})

