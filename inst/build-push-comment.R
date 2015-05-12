# -----------------------------------------------------------------------
# Travis does two types of builds:
#
# (1) A so-called "push". This essentially does a checkout on the most
# recent commit of the pull request, but *doesn't* merge with master.
# In this case, $TRAVIS_PULL_REQUEST = "false"
# (2) A so-called "pr" (pull request). This *does* merge with master.
# In this case, $TRAVIS_PULL_REQUEST contains the pull request number.
#
# Since it makes more sense to visually compared what we'd see *after* we
# merge with master, we don't do anything here if it's a push build.
# -----------------------------------------------------------------------

# Read more about Travis environment variables --
# http://docs.travis-ci.com/user/ci-environment/#Environment-variables
tpr <- Sys.getenv("TRAVIS_PULL_REQUEST")
if (tpr != "false" && tpr != "") {
  library("httr")
  library("testthat")
  # gistr is a good reference for talking to the github API via httr
  # https://github.com/ropensci/gistr/blob/master/R/zzz.R
  base <- 'https://api.github.com/repos/ropensci/plotly/'
  header <- add_headers(`User-Agent` = "plotly",
                        `Accept` = 'application/vnd.github.v3+json',
                        `Authorization` = paste0("token ", Sys.getenv("GH_TOKEN")))
  # Grab the branch name for this pull request (must be successful!!)
  # http://stackoverflow.com/questions/15096331/github-api-how-to-find-the-branches-of-a-pull-request
  pr <- sprintf(paste0(base, 'pulls/%s'), tpr)
  res <- GET(url = pr, header)
  stop_for_status(res)
  info <- content(res)
  branch <- strsplit(info$head$label, ":")[[1]][2]

  # Return an abbreviated version of a hash
  abbrev_hash <- function(hash = "") substr(hash, 1, 7)

  # Grab HEAD info for each branch (this might not be necessary)
#   br <- paste0(base, 'branches')
#   res <- GET(br)
#   stop_for_status(res)
#   info <- content(res)
#   commits <- sapply(info, "[[", "commit")
#   shas <- unlist(commits["sha",])
#   shas <- sapply(shas, abbrev_hash, USE.NAMES = FALSE)
#   shas <- setNames(shas, sapply(info, "[[", "name"))

  # NOTE: $TRAVIS_COMMIT doesn't match the HEAD of this (or master) branch!!!
  # Remember that we're *simulating* a merge with master, but the hash for the
  # *actual* merge will be different. Instead of installing master each time
  # we call save_outputs(), we install once here, if necessary, and re-run tests
  this_hash <- abbrev_hash(Sys.getenv("TRAVIS_COMMIT"))
  base_hash <- abbrev_hash(info$base$sha)
  head_hash <- abbrev_hash(info$head$sha)
  test_rerun <- function(hash) {
    if (!hash %in% dir("plotly-test-table/R")) {
      devtools::install_github("ropensci/plotly", ref = hash, local = FALSE)
      message("Rerunning tests")
      setwd("plotly/tests"); source("testthat.R")
    }
  }
  test_rerun(this_hash)
  test_rerun(base_hash)

  # TODO: Remove plotly-test-table folders that are no longer needed
  # by comparing the directories to branch HEADs for plotly. This could
  # be hard to do the git rm properly. We could also run tests for missing
  # HEAD shas, but that's probably overkill

  # list png files in a particular directory
  pngs <- function(...) {
    dir(file.path("plotly-test-table", "R", ...),
        pattern = "\\.png$", full.names = T)
  }
  # Build the main HTML page for this build
  ggpngs <- pngs("ggplot2", packageVersion("ggplot2"))
  df <- data.frame(sub("\\.png$", "", basename(ggpngs)),
                   ggpngs, pngs(this_hash), pngs(base_hash))
  names(df) <- c("test", "ggplot2", branch, "master")
  # TODO: create an HTML page for each test
  df$test <- sprintf('<a href = "%s.html"> %s </a>', df$test)
  for (i in setdiff(names(df), "test"))
    df[, i] <- sprintf('<a href = "%s"> <img src = "%s"> </a>', df[, i])
  test_table <- knitr::knit2html(text = '`r knitr::kable(df, type = "html")`',
                                 quiet = TRUE)
  dest <- file.path("plotly-test-table", "R", this_hash, "index.html")
  writeLines(test_table, dest)

  # TODO:
  # * convert for thumbnails!! (see wch/vtest's convert_png() for alternative)
  # * create home page for R with commmit info -- https://developer.github.com/v3/git/commits/

  # add, commit, push to gh-pages branch of plotly-test-table
  system("git add *")
  build_link <- paste0('https://travis-ci.org/ropensci/plotly/builds/',
                       Sys.getenv("TRAVIS_BUILD_ID"))
  commit_msg <- paste0('"Pushed from ', build_link, '"')
  system(paste('git commit -m', commit_msg))
  # This post explains how this works -- http://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html
  repo <- sprintf("https://%s@github.com/cpsievert/plotly-test-table.git", Sys.getenv("GH_TOKEN"))
  system(paste("git pull -q", repo, "gh-pages"))
  system(paste("git push -q", repo, "gh-pages"))

  # post comment if a link to this SHA doesn't exist
  # (needed since Travis randomly re-builds stuff)
  tbl_link <- sprintf("http://cpsievert.github.io/plotly-test-table/R/%s/index.html", this_hash)
  msg <- sprintf("On TravisCI, commit %s was successfully merged with %s (master) to create %s. A visual testing table comparing %s with %s can be found here:\n %s",
                 head_hash, base_hash, this_hash, base_hash, this_hash, tbl_link)
  msg <- paste("> The message below was automatically generated after build", build_link, "\n\n", msg)
  commentz <- sprintf(paste0(base, 'issues/%s/comments'), tpr)
  res <- GET(commentz, header)
  warn_for_status(res)
  info <- content(res)
  old_body <- unlist(lapply(info, "[", "body"))
  if (!any(grepl(tbl_link, old_body))) {
    json <- jsonlite::toJSON(list(body = msg), auto_unbox = TRUE)
    POST(url = commentz, header, body = json, encode = "json")
  } else {
    message("Link already posted")
  }
} else {
  message('The test table is only built during the "pull request" build.')
}


# IDEAS:
# * iframe into json diffs on github???
# * Github now renders IPython!!!
