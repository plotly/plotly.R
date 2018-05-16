library("testthat")
library("plotly")
library("RSclient")

# report any differences in plot (list) hashes if this is a Travis pull request
report_diffs <- grepl("^[0-9]+$", Sys.getenv("TRAVIS_PULL_REQUEST"))
# build a ggplot2 comparison table?
build_table <- Sys.getenv("PLOTLY_TABLE") == "TRUE"

# stuff that should be done once (not everytime save_outputs() is called)
if (report_diffs || build_table) {
  message("Spinning up an independent R session with plotly's master branch installed")
  Rserve::Rserve(args = "--vanilla --RS-enable-remote")
  conn <- RSconnect()
  # ensure the seed is the same for randomized tests
  set.seed(1)
  RSeval(conn, "set.seed(1)")
  # we don't make assumptions about ggplot2 versioning,
  # but it is _strongly_ recommended to use the CRAN version (of ggplot2)
  RSeval(conn, "devtools::install_github('ropensci/plotly')")
  RSeval(conn, "library(plotly)")
  if (report_diffs) {
    # hash of the version being tested
    this_hash <- substr(Sys.getenv("TRAVIS_COMMIT"), 1, 7)
    # hash of version to compare with (master)
    master_hash <- RSeval(conn, "packageDescription('plotly')$GithubSHA1")
    master_hash <- substr(master_hash, 1, 7)
    # plotly-test-table repo hosts the diff pages & keeps track of previous versions
    table_dir <- normalizePath("../../plotly-test-table", mustWork = T)
    # Make sure we have appropriate versions of plotlyjs
    # (see plotly-test-table/template/template/index.html)
    file.copy(
      file.path("..", "inst", "htmlwidgets", "lib", "plotlyjs", "plotly-latest.min.js"),
      file.path(table_dir, "template", "New.min.js"),
      overwrite = TRUE
    )
    download.file(
      "https://raw.githubusercontent.com/ropensci/plotly/master/inst/htmlwidgets/lib/plotlyjs/plotly-latest.min.js", 
      file.path(table_dir, "template", "Old.min.js"),
      method = "curl"
    )
    # directory for placing test differences
    this_dir <- file.path(table_dir, this_hash)
    if (dir.exists(this_dir)) {
      message("Tests were already run on this commit. Nuking the old results...")
      unlink(this_dir, recursive = T)
    }
    master_dir <- file.path(table_dir, master_hash)
    # csv file that tracks plot hashes
    hash_file <- file.path(table_dir, "hashes.csv")
    if (!file.exists(hash_file)) {
      file.create(hash_file)
      cat("commit,test,hash\n", file = hash_file, append = T)
    }
    hash_info <- utils::read.csv(hash_file)
    master_info <- hash_info[hash_info$commit %in% master_hash, ]
  }
}

# Some tests make plot.ly HTTP requests and require a valid user account
# (see test-plotly-filename.R). For security reasons, these tests should be 
# skipped on pull requests (the .travis.yml file uses encrypted credentials
# & encrypted environment vars cannot be accessed on pull request builds)
skip_if_not_master <- function() {
  is_pr <- grepl("^[0-9]+$", Sys.getenv("TRAVIS_PULL_REQUEST"))
  is_r_release <- Sys.getenv("TRAVIS_R_VERSION_STRING", "release") == "release"
  if (!is_pr && is_r_release) return(invisible(TRUE))
  skip("plot.ly API calls are only tested on the master build on r-release")
}

# This function is called within testthat/test-*.R files.
# It takes a ggplot or plotly object as input, and it returns a figure
# object (aka the data behind the plot).
save_outputs <- function(gg, name) {
  print(paste("Running test:", name))
  p <- plotly_build(gg)$x[c("data", "layout")]
  has_diff <- if (report_diffs) {
    # save a hash of the R object
    plot_hash <- digest::digest(p)
    info <- paste(this_hash, name, plot_hash, sep = ",")
    cat(paste(info, "\n"), file = hash_file, append = T)
    test_info <- master_info[master_info$test %in% name, ]
    # is the plot hash different from master?
    !isTRUE(plot_hash == test_info$hash)
  } else FALSE
  if (has_diff || build_table) {
    RSassign(conn, gg)
    pm <- RSeval(conn, "tryCatch(plotly::plotly_build(gg)$x[c('data', 'layout')], error = function(e) e$message)")
    if (build_table) {
      # save pngs of ggplot
      filename <- paste0(gsub("\\s+", "-", name), ".png")
      ggFile <- paste("ggplot", filename, sep = "-")
      res <- tryCatch(ggsave(ggFile, gg), 
                      error = function(e) {
                        err <- ggplot() + 
                          annotate('text', label = paste('Error:', e$message), 
                                   x = 1, y = 1, color = 'red')
                        ggsave(ggFile, err, width = 3, height = 2, units = 'in')
                      })
      img <- function(x, f) {
        tryCatch(plotly_IMAGE(x, out_file = f, width = 300, height = 400),
                 error = function(e) {
                   err <- ggplot() + 
                     annotate('text', label = paste('Error:', e$message), 
                              x = 1, y = 1, color = 'red')
                   # TODO: convert pixels to inches?
                   ggsave(plotlyFile, err, width = 3, height = 2, units = 'in')
                 })
      }
      # save _this_ plotly version
      plotlyFile <- paste("plotly", this_hash, filename, sep = "-")
      res <- img(p, plotlyFile)
      # save _master_ plotly version
      plotlyFile <- paste("plotly", "master", filename, sep = "-")
      RSassign(conn, plotlyFile, "plotlyFile")
      RSassign(conn, img, "img")
      res <- RSeval(conn, "img(p, plotlyFile)")
    }
    # it could be that the hash didn't exist, so make sure they're different
    # before build a diff page
    if (plot_hash != digest::digest(pm)) {
      test_dir <- file.path(this_dir, gsub("\\s+", "-", name))
      if (dir.exists(test_dir)) stop(shQuote(name), " has already been used to save_outputs() in another test.")
      dir.create(test_dir, recursive = T)
      # copy over diffing template
      file.copy(
        dir(file.path(table_dir, "template", "template"), full.names = TRUE), test_dir
      )
      # overwrite the default JSON
      writeLines(
        paste("New =", plotly:::to_JSON(p)), 
        file.path(test_dir, "New.json")
      )
      writeLines(
        paste("Old =", plotly:::to_JSON(pm)), 
        file.path(test_dir, "Old.json")
      )
    }
  }
  p
}

# use me just like testthat::test_check()
test_run <- function(...) {
  # shut down the other R session on exit
  if (report_diffs || build_table) {
    on.exit(RSshutdown(conn))
    on.exit(RSclose(conn), add = TRUE)
  }
  test_check(...)
}

test_run("plotly")

# now, actually build the table (if necessary)
if (build_table) {
  imgfy <- function(pat) {
    sprintf(
      "<img src='%s' width='%s' height='%s' />", 
      dir(pattern = pat), width, height
    )
  }
  imgs <- data.frame(
    imgfy("^ggplot-"),
    imgfy(paste0("^plotly-", this_hash)),
    imgfy("^plotly-master")
  )
  imgs <- cbind(
    sub("\\.png", "", sub("^[a-z]+1-", "", dir(pattern = "^ggplot-"))),
    imgs
  )
  setNames(imgs, c("ggplot2", this_hash, "master"))
  
  html <- sprintf(
    '<!DOCTYPE html>
    <html>
     <head>
      <meta charset=\"utf-8\"/>
      <style type=\"text/css\" media=\"screen\"> table td tr { border:1px solid #FF0000;} </style>
     </head>
     <body>
      %s
     </body>
    </html>', as.character(knitr::kable(imgs, format = "html", escape = FALSE)))
  tbl <- file.path(tmpDir, "index.html")
  writeLines(html, tbl)
  browseURL(tbl)
  invisible(tbl)
}
