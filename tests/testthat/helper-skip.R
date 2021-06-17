r_version <- paste(R.version$major, R.version$minor, sep = ".")
r_release <- rversions::r_release()$version
is_release <- isTRUE(r_release == r_version)
is_win <- .Platform$OS.type == "windows"

skip_cloud_tests <- function() {
  skip_on_cran()
  if (is.na(Sys.getenv("plotly_username", NA))) {
    skip("Cloud testing requires a plotly account (plotly_username)")
  }
  if (is.na(Sys.getenv("plotly_api_key", NA))) {
    skip("Cloud testing requires a plotly account (plotly_api_key)")
  }
  if (!is_release || !is_win) {
    skip("Cloud testing is only run on Windows with the current release of R")
  }
}

skip_shinytest_tests <- function() {
  skip_on_cran()
  skip_if_not_installed("shinytest")
  if (!grepl("true", Sys.getenv("SHINYTEST"), fixed = TRUE)) {
    skip("shinytest testing requires the SHINYTEST environment variable to be true")
  }
}
