#' @param gg a ggplot object
#' @param name name of the test

save_outputs <- function(gg, name) {
  # only render/save pngs if this is a Travis pull request
  # (see build-comment-push.R for better explanation of this logic)
  tpr <- Sys.getenv("TRAVIS_PULL_REQUEST")
  if (tpr != "false" && tpr != "") {
    table_dir <- file.path(Sys.getenv("TRAVIS_BUILD_DIR"), "plotly-test-table")
    # this environment variable should be set by testthat.R
    plotly_dir <- file.path(table_dir, "R", Sys.getenv("plotly-hash"))
    gg_dir <- file.path(table_dir, "R", "ggplot2")
    if (!dir.exists(plotly_dir)) dir.create(plotly_dir, recursive = TRUE)
    if (!dir.exists(gg_dir)) dir.create(gg_dir, recursive = TRUE)

    # If we don't have pngs for this version (of ggplot2), generate them;
    # otherwise, generate plotly pngs
    # NOTE: we can't save both plotly & ggplot2 at once since R CMD check
    # suppresses output and travis has 10 minute time limit
    # https://github.com/travis-ci/travis-ci/issues/3849
    ggversion <- as.character(packageVersion("ggplot2"))
    if (!ggversion %in% dir(gg_dir)) {
      dest <- file.path(gg_dir, ggversion, paste0(name, ".png"))
      e <- try(gg, silent = TRUE)
      png(filename = dest)
      if (inherits(e, "try-error")) plot(1, type="n"); text(1, "ggplot2 error") else gg
      dev.off()
    } else  {
      # TODO: could speed things up by avoiding two calls to gg2list()
      # (this will require tweaking expect_traces())
      p <- plotly(gg, browse = FALSE)
      png_url <- paste0(p[["url"]], ".png")
      resp <- httr::GET(png_url)
      # print the response if it wasn't successful
      if (httr::warn_for_status(resp)) resp
      # write png version of plotly figure to disk
      writeBin(httr::content(resp, as = "raw"),
               file.path(plotly_dir, paste0(name, ".png")))
    }
  }
  invisible(NULL)
}

# NOTE: I'm assumming Travis is installing most recent ggplot2 off CRAN
# Here is one way to get current ggplot2 version off of CRAN if need be
# gg <- rvest::html("http://cran.r-project.org/web/packages/ggplot2/")
# tab <- rvest::html_table(gg, header = FALSE)[[1]]
# ggversion <- tab[grepl("Version:", tab[, 1]), 2]
