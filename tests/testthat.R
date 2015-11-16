library("testthat")
library("plotly")
library("RSclient")

# is this a pull request? if so, we compare results from this test with master
check_tests <- grepl("^[0-9]+$", Sys.getenv("TRAVIS_PULL_REQUEST"))

# objects that should only be created once
if (check_tests) {
  message("Spinning up an independent R session with plotly's master branch installed")
  Rserve::Rserve(args = "--vanilla --RS-enable-remote")
  conn <- RSconnect()
  # some tests use randomized data
  set.seed(1)
  RSeval(conn, "set.seed(1)")
  RSeval(conn, "library(methods); devtools::install_github('ropensci/plotly')")
  # hash of the version being tested
  this_hash <- substr(Sys.getenv("TRAVIS_COMMIT"), 1, 7)
  # hash of version to compare with (master)
  master_hash <- RSeval(conn, "packageDescription('plotly')$GithubSHA1")
  master_hash <- substr(master_hash, 1, 7)
  # plotly-test-table repo hosts the diff pages & keeps track of previous versions
  table_dir <- normalizePath("../../plotly-test-table", mustWork = T)
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

# This function is called within testthat/test-*.R files.
# It takes a ggplot or plotly object as input, and it returns a figure
# object (aka the data behind the plot).
save_outputs <- function(gg, name) {
  print(paste("Running test:", name))
  p <- plotly_build(gg)
  if (check_tests) {
    # save a hash of the R object
    plot_hash <- digest::digest(p)
    info <- paste(this_hash, name, plot_hash, sep = ",")
    cat(paste(info, "\n"), file = hash_file, append = T)
    # if the plot hash is different from master, build using the master branch
    test_info <- master_info[master_info$test %in% name, ]
    if (!isTRUE(plot_hash == test_info$hash)) {
      # hack to transfer workspace to the other R session
      rs_assign <- function(obj, name) RSassign(conn, obj, name)
      res <- mapply(rs_assign, mget(ls()), ls())
      # also need to transfer over the plotly environment to enable NSE
      res <- RSassign(conn, plotly:::plotlyEnv, "plotlyEnv")
      res <- RSeval(conn, "unlockBinding('plotlyEnv', asNamespace('plotly'))")
      res <- RSeval(conn, "assign('plotlyEnv', plotlyEnv, pos = asNamespace('plotly'))")
      pm <- RSeval(conn, "plotly::plotly_build(gg)")
      # it could be that the hash didn't exist, so make sure they're different
      if (plot_hash != digest::digest(pm)) {
        test_dir <- file.path(this_dir, gsub("\\s+", "-", name))
        if (dir.exists(test_dir)) stop(shQuote(name), " has already been used to save_outputs() in another test.")
        dir.create(test_dir, recursive = T)
        # copy over diffing template
        file.copy(
          file.path(table_dir, "template", "template", "index.html"), 
          test_dir, 
          recursive = T
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
  }
  p
}


test_check("plotly")

# shut down the other R session
if (check_tests) {
  RSshutdown(conn)
  RSclose(conn)
}
