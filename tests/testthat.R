library("testthat")
library("plotly")

# hash of the version being tested
this_hash <- substr(Sys.getenv("TRAVIS_COMMIT"), 1, 7)
# we may want to compare results with master (if this is a Travis pull request)
master_hash <- substr(system("git rev-parse master", intern = T), 1, 7)
check_tests <- grepl("^[0-9]+$", Sys.getenv("TRAVIS_PULL_REQUEST"))

# plotly-test-table repo hosts the diff pages & keeps track of previous versions
table_dir <- normalizePath("../../plotly-test-table")
this_dir <- file.path(table_dir, "R", this_hash)
master_dir <- file.path(table_dir, "R", master_hash)

# text file that tracks plot hashes
hash_file <- file.path(table_dir, "R", "hashes.csv")
if (!file.exists(hash_file)) {
  file.create(hash_file)
  cat("commit,test,hash\n", file = hash_file, append = TRUE)
}
hash_info <- utils::read.csv(hash_file)
master_info <- hash_info[hash_info$commit %in% master_hash, ]

# This function is called within testthat/test-*.R files.
# It takes a ggplot or plotly object as input, and it returns a figure
# object (aka the data behind the plot).
# Along the way, if this is a pull request build on Travis,
# it will POST figures to plotly and save pngs 
save_outputs <- function(gg, name) {
  print(paste("Running test:", name))
  p <- plotly_build(gg)
  if (check_tests) {
    # save a hash of the R object
    plot_hash <- digest::digest(p)
    info <- paste(this_hash, name, plot_hash, sep = ",")
    cat(paste(info, "\n"), file = hash_file, append = TRUE)
    # if the plot hash is different from master, build using the master branch
    test_info <- master_info[master_info$test %in% name, ]
    if (!isTRUE(plot_hash == test_info$hash)) {
      pm <- run_master(gg)
      # it could be that the hash didn't exist, so make sure they're different
      browser()
      if (plot_hash != digest::digest(pm)) {
        if (packageVersion("plotly") < "1.0.8") stop("These tests assume you're running plotly version 1.0.8 or higher", call. = F)
        # copy over diffing template
        jsondiff <- dir("../inst/jsondiff", full.names = TRUE)
        test_dir <- file.path(this_dir, name)
        if (dir.exists(test_dir)) stop(shQuote(name), " has already been used to save_outputs() in another test.")
        print(test_dir)
        dir.create(test_dir)
        file.copy(jsondiff, test_dir, recursive = TRUE)
        # overwrite the default JSON
        writeLines(
          plotly:::to_JSON(p), 
          file.path(test_dir, "New.json")
        )
        writeLines(
          plotly:::to_JSON(pm), 
          file.path(test_dir, "Old.json")
        )
      }
    }
  }
  p
}

# a hack to run plotly_build() using code on the HEAD of master 
# (instead of the current version being tested)
run_master <- function(p) {
  saveRDS(p, "tmp.rds")
  system("git checkout master; Rscript -e 'devtools::load_all(); saveRDS(plotly::plotly_build(p <- readRDS(\"tmp.rds\")), \"tmp.rds\")'")
  on.exit(unlink("tmp.rds"))
  readRDS("tmp.rds")
}

test_check("plotly")
