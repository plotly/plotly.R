context("get_figure")

test_that("requests made by a user who doesn't exist error a 404", {
  py <- plotly("user_does_not_exist", "api_key_shouldnt_matter")
  expect_error({
    py$get_figure("get_test_user", 0)
  }, "404")
})

test_that("requests made to retrieve a file that doesn't error return a 404", {
  py <- plotly("get_test_user", "vgs6e0cnoi")
  expect_error({
    py$get_figure("get_test_user", 1000)
  }, "404")
})

test_that("requests made with the wrong API key error a 401", {
  py <- plotly("get_test_user", "some_invalid_api_key")
  expect_error({
    py$get_figure("get_test_user", 1)
  }, "401")
})

test_that("requests made to retrieve some elses private file errors a 403", {
  py <- plotly("get_test_user_2", "0f9es4r6tm")
  expect_error({
    py$get_figure("get_test_user", 1)
  }, "403")
})

test_that("requests made to retrieve some elses private file errors a 403", {
  py <- plotly("get_test_user_2", "0f9es4r6tm")
  expect_error({
    py$get_figure("get_test_user", 1)
  }, "403")
})

test_that("retrieving a public figure ... works.", {
  py <- plotly("get_test_user_2", "0f9es4r6tm")
  figure <- py$get_figure("get_test_user", 0)
  expect_equivalent(figure$data[[1]]$x, list("1", "2", "3"))
})
