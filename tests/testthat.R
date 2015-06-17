library("testthat")
library("plotly")
# find the hash of the currently installed plotly package
pkg_info <- devtools::session_info()$packages
src <- subset(pkg_info, package == "plotly")$source
hash <- if (src == "local") {
  # you could also do `git rev-parse HEAD`, but this is cleaner for Travis
  substr(Sys.getenv("TRAVIS_COMMIT"), 1, 7)
} else {
  # thankfully devtools includes hash for packages installed off GitHub
  sub("\\)", "", strsplit(src, "@")[[1]][2])
}
table_dir <- file.path(Sys.getenv("TRAVIS_BUILD_DIR"), "..", "plotly-test-table")
plotly_dir <- file.path(table_dir, "R", hash)
if (!dir.exists(plotly_dir)) dir.create(plotly_dir, recursive = TRUE)

save_outputs <- function(gg, name) {
  # only render/save pngs if this is a Travis pull request
  # (see build-comment-push.R for better explanation of this logic)
  message("Running test: ", name)
  tpr <- Sys.getenv("TRAVIS_PULL_REQUEST")
  if (tpr != "false" && tpr != "") {
    resp <- (p <- ggplotly(gg))
    resp <- httr::GET(paste0(resp[["url"]], ".png"))
    # print the response if it wasn't successful
    if (httr::warn_for_status(resp)) resp
    # write png version of plotly figure to disk
    filename <- file.path(plotly_dir, paste0(name, ".png"))
    writeBin(httr::content(resp, as = "raw"), filename)
  }
  # eventually change tests so that they use output from this function
  invisible(p)
  #gg_dir <- file.path(table_dir, "R", "ggplot2")
  #if (!dir.exists(gg_dir)) dir.create(gg_dir, recursive = TRUE)
  
  # If we don't have pngs for this version (of ggplot2), generate them;
  # otherwise, generate plotly pngs
  # NOTE: we can't save both plotly & ggplot2 at once since R CMD check
  # suppresses output and travis has 10 minute time limit
  # https://github.com/travis-ci/travis-ci/issues/3849
  #     ggversion <- as.character(packageVersion("ggplot2"))
  #     if (!ggversion %in% dir(gg_dir)) {
  #       gglife <- file.path(gg_dir, ggversion)
  #       dir.create(gglife, recursive = TRUE)
  #       e <- try(gg, silent = TRUE)
  #       png(filename = file.path(gglife, paste0(name, ".png")))
  #       if (inherits(e, "try-error")) {
  #         plot(1, type="n")
  #         text(1, "ggplot2 error")
  #       } else gg
  #       dev.off()
  #     } else  {
  #     }
}

test_check("plotly")

# NOTE: I'm assumming Travis is installing most recent ggplot2 off CRAN
# Here is one way to get current ggplot2 version off of CRAN if need be
# gg <- rvest::html("http://cran.r-project.org/web/packages/ggplot2/")
# tab <- rvest::html_table(gg, header = FALSE)[[1]]
# ggversion <- tab[grepl("Version:", tab[, 1]), 2]
