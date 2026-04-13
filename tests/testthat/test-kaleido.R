test_that("newKaleidoScope does not inline Windows temp paths into Python code", {
  skip_if_not_installed("reticulate")
  skip_if_not(suppressWarnings(reticulate::py_available(TRUE)))

  win_path <- "C:\\users\\name\\AppData\\Local\\Temp\\Rtmp\\file.json"
  py_calls <- character()
  write_fig_args <- NULL

  testthat::local_mocked_bindings(
    plotly_build = function(...) {
      list(x = list(data = list(), layout = list(), config = list()))
    },
    to_JSON = function(...) "{}",
    plotlyMainBundlePath = function() "plotly.min.js",
    .package = "plotly"
  )

  testthat::local_mocked_bindings(
    tempfile = function(...) win_path,
    writeLines = function(...) NULL,
    .package = "base"
  )

  testthat::local_mocked_bindings(
    py_run_string = function(code, ...) {
      py_calls <<- c(py_calls, code)
      py_obj <- get("py", envir = asNamespace("reticulate"))
      py_obj$fig <- "fake-fig"
      invisible(NULL)
    },
    .package = "reticulate"
  )

  kaleido <- list(write_fig_sync = function(fig, file, opts, kopts) {
    write_fig_args <<- list(fig = fig, file = file, opts = opts, kopts = kopts)
    invisible(NULL)
  })

  scope <- newKaleidoScope(kaleido)
  scope$transform(list(), "figure.png")

  expect_identical(py_calls, "import json; fig = json.load(open(tmp_json_path))")
  expect_identical(write_fig_args$fig, "fake-fig")
  expect_identical(write_fig_args$file, "figure.png")
})
