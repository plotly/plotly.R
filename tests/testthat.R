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
plotly_thumb_dir <- file.path(plotly_dir, "thumbs")
dir.create(plotly_thumb_dir, showWarnings = FALSE, recursive = TRUE)

# in case we need save ggplot2 output
ggversion <- as.character(packageVersion("ggplot2"))
gg_dir <- file.path(table_dir, "R", paste0("ggplot2-", ggversion))
gg_thumb_dir <- file.path(gg_dir, "thumbs")
dir.create(gg_thumb_dir, showWarnings = FALSE, recursive = TRUE)
gg_names <- sub("\\.png$", "", dir(gg_dir, pattern = "\\.png$"))

# text file that tracks figure hashes
hash_file <- file.path(table_dir, "R", "hashes.csv")
if (!file.exists(hash_file)) {
  file.create(hash_file)
  cat("commit,test,hash\n", file = hash_file, append = TRUE)
}

# This function is called within testthat/test-*.R files.
# It takes a ggplot or plotly object as input, and it returns a figure
# object (aka the data behind the plot).
# Along the way, if this is a pull request build on Travis,
# it will POST figures to plotly and save pngs 
save_outputs <- function(gg, name) {
  print(paste("Running test:", name))
  p <- if (is.ggplot(gg)) gg2list(gg) else plotly_build(gg)
  tpr <- Sys.getenv("TRAVIS_PULL_REQUEST")
  # only create images if this is a Travis pull request
  if (tpr != "false" && tpr != "") {
    # save a hash of the R object sent to the plotly server
    # (eventually use this to prevent redundant POSTs?!)
    info <- paste(hash, name, digest::digest(p), sep = ",")
    cat(paste(info, "\n"), file = hash_file, append = TRUE)
    filename <- file.path(plotly_dir, paste0(name, ".png"))
    if (file.exists(filename))
      stop(shQuote(name), " has already been used to save_outputs() in another test.")
    # Using plotly_IMAGE() requires 1.0.8 or higher
    if (packageVersion("plotly") < "1.0.8") 
      stop("These tests assume you're running plotly version 1.0.8 or higher")
    resp <- plotly_IMAGE(p, height = 500, width = 700, out_file = filename)
    # now convert png to a smaller size
    argz <- c(filename, "-density", "36x36", "-write", 
              file.path(plotly_thumb_dir, paste0(name, ".png")), "+delete")
    system2("convert", argz)
    
    # if missing, save the ggplot2 version
    # do an else if to take advantage of both builds?
    if (!name %in% gg_names) {
      gg_file <- file.path(gg_dir, paste0(name, ".png"))
      png(gg_file, width = 700, height = 500)
      try(print(gg))
      dev.off()
      # now convert png to a smaller size
      argz <- c(gg_file, "-density", "72x72", "-write", 
                file.path(gg_thumb_dir, paste0(name, ".png")), "+delete")
      system2("convert", argz)
    }
  }
  p
}

tryWhile <- function(expr, times = 20) {
  e <- try(expr)
  while (inherits(e, "try-error") && times > 0) {
    Sys.sleep(0.5)
    times <- times - 1
    e <- try(expr)
  }
  e
}

test_check("plotly")
