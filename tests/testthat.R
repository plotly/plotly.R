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
# setup directory for placing files during tests
# (note the working directory should be /path/to/plotly/tests)
table_dir <- normalizePath("../../plotly-test-table")
plotly_dir <- file.path(table_dir, "R", hash)
if (!dir.exists(plotly_dir)) dir.create(plotly_dir, recursive = TRUE)

# in case we need save ggplot2 output
ggversion <- as.character(packageVersion("ggplot2"))
gg_dir <- file.path(table_dir, "R", paste0("ggplot2-", ggversion))
gg_names <- sub("\\.png$", "", dir(gg_dir, pattern = "\\.png$"))

# text file that tracks figure hashes
hash_file <- file.path(table_dir, "R", "hashes.csv")
if (!file.exists(hash_file)) {
  file.create(hash_file)
  cat("commit,test,hash,url\n", file = hash_file, append = TRUE)
}

# This function is called within testthat/test-*.R files.
# It takes a ggplot or plotly object as input, and it returns a figure
# object (aka the data behind the plot).
# Along the way, if this is a pull request build on Travis,
# it will POST figures to plotly and save pngs 
save_outputs <- function(gg, name) {
  print(paste("Running test:", name))
  p <- if (is.ggplot(gg)) gg2list(gg) else plotly:::eval_list(get_plot(gg))
  tpr <- Sys.getenv("TRAVIS_PULL_REQUEST")
  # only render/save pngs if this is a Travis pull request
  if (tpr != "false" && tpr != "") {
    # POST data to plotly and return the url
    u <- if (packageVersion("plotly") < 1) {
      py <- plotly(Sys.getenv("plotly_username"), Sys.getenv("plotly_api_key"))
      resp <- py$ggplotly(gg, kwargs = list(auto_open = FALSE))
      resp$response$url
    } else {
      resp <- plotly_POST(p)
      resp$url
    }
    # save png under a directory specific to this installed version of plotly
    resp <- httr::GET(paste0(u, ".png"))
    httr::warn_for_status(resp)
    filename <- file.path(plotly_dir, paste0(name, ".png"))
    writeBin(httr::content(resp, as = "raw"), filename)
    # save a hash of the R object sent to the plotly server
    info <- paste(hash, name, digest::digest(p), u, sep = ",")
    cat(paste(info, "\n"), file = hash_file, append = TRUE)
  } else if (!name %in% gg_names) { # do we need to save a ggplot2 result?
    e <- try(gg, silent = TRUE)
    png(filename = file.path(gg_dir, paste0(name, ".png")))
    if (inherits(e, "try-error")) {
      plot(1, type = "n")
      text(1, "ggplot2 error")
    } else gg
    dev.off()
  }
  p
}

test_check("plotly")



# Keep database for tracking plot hashes? 
# Pros: (1) Don't have to upload a plot if underlying data hasn't changed
# Cons: (1) Significantly more complicated and leaves us prone to mistakes
# db <- if ("db.rds" %in% dir(r_dir)) {
#   readRDS("db.rds")
# } else {
#   data.frame(
#     commit = character(),
#     name = character(),
#     plot = character(),
#     stringsAsFactors = FALSE
#   )
# }
#df[nrow(df) + 1, ] <- c(hash, name, digest::digest(p))
