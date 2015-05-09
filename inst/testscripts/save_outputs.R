#' @param gg a ggplot object
#' @param name name of the test
#' @param ignore ignore ggplot2 errors?

save_outputs <- function(gg, name, ignore = FALSE) {
  # message(paste("running", name))
  # http://docs.travis-ci.com/user/ci-environment/#Environment-variables
  build_dir <- Sys.getenv("TRAVIS_BUILD_DIR")
  # only create plotlys if we're on travis
  if (build_dir != "") {
    table_dir <- file.path(build_dir, "plotly-test-table")
    # find the hash of the currently installed plotly package
    pkg_info <- devtools::session_info()$packages
    src <- subset(pkg_info, package == "plotly")$source
    hash <- sub("\\)", "", strsplit(src, "@")[[1]][2])

    # TODO: could speed things up by avoiding two calls to gg2list()
    # (this will require tweaking expect_traces())
    p <- plotly(gg, browse = FALSE)
    png_url <- paste0(p[["url"]], ".png")
    resp <- httr::GET(png_url)
    # print the response if it wasn't successful
    if (httr::warn_for_status(resp)) resp
    # write png version of plotly figure to disk
    dest <- file.path(table_dir, hash, paste0(name, ".png"))
    writeBin(content(resp, as = "raw"), dest)

    # if we don't have the results for this version (of ggplot2), save them
    ggversion <- packageVersion("ggplot2")
    gg_dir <- file.path(table_dir, "ggplot2")
    if (!ggversion %in% dir(gg_dir) && !ignore) {
      dest <- file.path(table_dir, "ggplot2", ggversion)
      ggsave(filename = paste0(name, ".png"), plot = gg, path = dest)
    }
  }
  invisible(NULL)
}

# NOTE: I'm assumming Travis is installing most recent ggplot2 off CRAN
# Here is one way to get current ggplot2 version off of CRAN if need be
# gg <- rvest::html("http://cran.r-project.org/web/packages/ggplot2/")
# tab <- rvest::html_table(gg, header = FALSE)[[1]]
# ggversion <- tab[grepl("Version:", tab[, 1]), 2]
