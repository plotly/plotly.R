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
if (dir.exists(this_dir)) {
  message("Tests were already run on this commit. Nuking the old results...")
  unlink(this_dir, recursive = T)
}
master_dir <- file.path(table_dir, "R", master_hash)

# text file that tracks plot hashes
r_dir <- file.path(table_dir, "R")
if (!dir.exists(r_dir)) dir.create(r_dir)
hash_file <- file.path(r_dir, "hashes.csv")
if (!file.exists(hash_file)) {
  file.create(hash_file)
  cat("commit,test,hash\n", file = hash_file, append = T)
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
    cat(paste(info, "\n"), file = hash_file, append = T)
    # if the plot hash is different from master, build using the master branch
    test_info <- master_info[master_info$test %in% name, ]
    if (!isTRUE(plot_hash == test_info$hash)) {
      # ugly hack to run plotly_build() using code on the HEAD of master 
      # (instead of the current version being tested)
      assign("gg", gg, envir = .GlobalEnv)
      assign("plotlyEnv", plotly:::plotlyEnv, envir = .GlobalEnv)
      save.image("tmp.rda")
      system("Rscript -e 'devtools::load_all(\"../../plotlyMaster\"); load(\"tmp.rda\"); saveRDS(plotly::plotly_build(gg), \"tmp.rds\")'")
      pm <- readRDS("tmp.rds")
      # it could be that the hash didn't exist, so make sure they're different
      if (plot_hash != digest::digest(pm)) {
        if (packageVersion("plotly") < "1.0.8") stop("These tests assume you're running plotly version 1.0.8 or higher", call. = F)
        # copy over diffing template
        jsondiff <- dir("../../inst/jsondiff", full.names = T)
        test_dir <- file.path(this_dir, name)
        if (dir.exists(test_dir)) stop(shQuote(name), " has already been used to save_outputs() in another test.")
        print(test_dir)
        dir.create(test_dir, recursive = T)
        file.copy(jsondiff, test_dir, recursive = T)
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
