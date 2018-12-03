# Some tests make plot.ly HTTP requests and require a valid user account
# (see test-plotly-filename.R). For security reasons, these tests should be 
# skipped on pull requests (the .travis.yml file uses encrypted credentials
# & encrypted environment vars cannot be accessed on pull request builds)
skip_if_not_master <- function() {
  if (is.na(Sys.getenv("plotly_username", NA))) {
    return(skip("Testing plot.ly API calls requires a plotly account"))
  }
  is_pr <- grepl("^[0-9]+$", Sys.getenv("TRAVIS_PULL_REQUEST"))
  is_r_release <- Sys.getenv("TRAVIS_R_VERSION_STRING", "release") == "release"
  if (!is_pr && is_r_release) return(invisible(TRUE))
  skip("plot.ly API calls are only tested on the master build on r-release")
}
