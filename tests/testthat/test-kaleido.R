test_that("newKaleidoScope does not inline Windows temp paths into Python code", {
  skip_if_not_installed("reticulate")
  skip_if_not(suppressWarnings(reticulate::py_available(TRUE)))
  withr::defer({
    py <- reticulate::py
    for (name in c("fig", "tmp_json_path")) {
      if (reticulate::py_has_attr(py, name)) {
        reticulate::py_del_attr(py, name)
      }
    }
  })

  win_path <- "C:\\users\\name\\AppData\\Local\\Temp\\Rtmp\\file.json"
  py_calls <- character()
  write_fig_args <- NULL

  local({
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
      unlink = function(...) 0,
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

    scope <- plotly:::newKaleidoScope(kaleido)
    scope$transform(list(), "figure.png")
  })

  expect_identical(py_calls[[1]], "import json; fig = json.load(open(tmp_json_path))")
  expect_true(!grepl(win_path, py_calls[[1]], fixed = TRUE))
  if (length(py_calls) >= 2) {
    expect_identical(py_calls[[2]], "del tmp_json_path")
  }
  expect_identical(write_fig_args$fig, "fake-fig")
  expect_identical(write_fig_args$file, "figure.png")
})

test_that("py_run_string_with_context rejects invalid Python identifiers", {
  skip_if_not_installed("reticulate")

  expect_error(
    plotly:::.py_run_string_with_context(
      "value = 1",
      context = list("bad name" = 1)
    ),
    "`context` names must be valid Python identifiers\\."
  )
})

test_that("py_run_string_with_context cleans up partial assignments", {
  skip_if_not_installed("reticulate")
  skip_if_not(suppressWarnings(reticulate::py_available(TRUE)))

  deleted <- character()

  testthat::local_mocked_bindings(
    py_has_attr = function(x, name) FALSE,
    py_get_attr = function(x, name, silent = FALSE) stop("unexpected py_get_attr call"),
    py_set_attr = function(x, name, value) {
      if (identical(name, "second")) {
        stop("boom")
      }
      invisible(NULL)
    },
    py_del_attr = function(x, name) {
      deleted <<- c(deleted, name)
      invisible(NULL)
    },
    py_run_string = function(code, local = FALSE, convert = TRUE) {
      stop("unexpected py_run_string call")
    },
    .package = "reticulate"
  )

  expect_error(
    plotly:::.py_run_string_with_context(
      "value = 1",
      context = list(first = 1, second = 2)
    ),
    "boom"
  )
  expect_identical(deleted, "first")
})
