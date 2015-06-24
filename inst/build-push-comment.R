# -----------------------------------------------------------------------
# This script will build a visual testing table for comparing ggplot and plotly
# figures. Travis does two types of builds:
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
library("httr")
library("testthat")

# http://docs.travis-ci.com/user/ci-environment/#Environment-variables
build_link <- file.path('https://travis-ci.org/ropensci/plotly/builds',
                        Sys.getenv("TRAVIS_BUILD_ID"))
tpr <- Sys.getenv("TRAVIS_PULL_REQUEST")
# is this build a pull request, build the test table
if (tpr != "false" && tpr != "") {
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
  
  # NOTE: $TRAVIS_COMMIT won't match the HEAD of this (or the master) branch!!!
  # (since this is a pull requst, we're *simulating* a merge with master)
  this_hash <- abbrev_hash(Sys.getenv("TRAVIS_COMMIT"))
  this_dir <- file.path("R", this_hash)
  this_pngs <- dir(this_dir, pattern = "\\.png$")
  
  # HEAD of the master branch. 
  base_hash <- abbrev_hash(info$base$sha)
  base_dir <- file.path("R/", base_hash)
  base_pngs <- dir(base_dir, pattern = "\\.png$")
  # Re-run (current) test suite with master branch if it's missing any tests 
  if (!all(this_pngs %in% base_pngs)) {
    dir.create(base_dir, showWarnings = FALSE)
    devtools::install_github("ropensci/plotly", ref = base_hash)
    print("Rerunning tests with master")
    try(source("../plotly/tests/testthat.R", chdir = TRUE))
  }

  # ---------------------------------------------------------------------------
  # Build the main HTML page for this build
  # ---------------------------------------------------------------------------
  df <- data.frame(
    sub("\\.png$", "", this_pngs),
    file.path(gg_dir, this_pngs),
    file.path(this_dir, this_pngs),
    file.path(base_dir, this_pngs),
    stringsAsFactors = FALSE
  )
  names(df) <- c("test", "ggplot2", branch, "master")
  df$test <- sprintf('<a href = "%s.html"> %s </a>', df$test, df$test)
  for (i in setdiff(names(df), "test"))
    df[, i] <- sprintf('<a href = "%s"> <img src = "%s"> </a>', df[, i], df[, i])
  print(df)
  test_table <- knitr::knit2html(text = '`r knitr::kable(df, type = "html")`',
                                 quiet = TRUE)
  dest <- file.path(this_hash, "index.html")
  writeLines(test_table, dest)
  
  # start constructing automated GitHub message 
  tbl_link <- sprintf("http://cpsievert.github.io/plotly-test-table/R/%s/", this_hash)
  msg1 <- paste("> The message below was automatically generated after build", build_link, "\n\n")
  msg2 <- sprintf("On TravisCI, commit %s was successfully merged with %s (master) to create %s. A visual testing table comparing %s with %s can be found here:\n %s",
                  abbrev_hash(info$head$sha), base_hash, this_hash, base_hash, this_hash, tbl_link)
  # ---------------------------------------------------------------------------
  # For each test, build a webpage (under this commit hash directory)
  # If plot hashed were different, include a JSON diff and links in the github comment
  # ---------------------------------------------------------------------------
  hashes <- read.csv("R/hashes.csv")
  hashes <- hashes[hashes$commit %in% c(this_hash, base_hash), ]
  diffs <- character()
  for (i in df$test) {
    test_info <- hashes[hashes$test %in% i, ]
    # are the plot hashes different for this test?
    has_diff <- length(unique(test_info$hash)) > 1
    # TODO: add the ggplot result to this page?
    top <- sprintf(
      '<a src="%s"> <img src="%s.png"> </a> <a src="%s"> <img src="%s.png"> </a>',
      test_info$url[1], urls[1], test_info$url[2], urls[2]
    )
    bottom <- if (has_diff) {
      diffs[[i]] <- 1
      sprintf(
        ' \n `r jsdiff::jsdiff(get_plot(get_figure(url=%s)), get_plot(get_figure(url=%s)))`',
        test_info$url[1], test_info$url[2]
      )
    } else {
      sprintf('\n No difference in this test between %s and %s', this_hash, base_hash)
    }
    diff_table <- knitr::knit2html(text = paste(top, bottom))
    name_dir <- sprintf("R/%s/%s", this_hash, i)
    dir.create(name_dir, recursive = TRUE)
    writeLines(diff_table, paste0(name_dir, "/index.html"))
  }
  msg3 <- sprintf("Detected a total of %s differences: \n", length(diffs))
  msg4 <- paste(tbl_link, names(diffs), collapse = " \n ")
  msg <- paste(msg1, msg2, msg3, msg4)
  commentz <- sprintf(paste0(base, 'issues/%s/comments'), tpr)
  res <- GET(commentz, header)
  warn_for_status(res)
  old_body <- unlist(lapply(content(res), "[", "body"))
  # only post a comment if this hash doesn't appear in any of the comments
  # (needed since Travis randomly re-builds stuff)
  if (!any(grepl(this_hash, old_body))) {
    json <- jsonlite::toJSON(list(body = msg), auto_unbox = TRUE)
    POST(url = commentz, header, body = json, encode = "json")
  } else {
    message("Link already posted")
  }
}

st <- system("git status", intern = TRUE)
# if the working state is dirty, clean it, and push!
# (if tests are added, or if ggplot2 updates, the push travis build will add ggplot2 pngs)
if (any(grepl("modified:", st))) {
  system("git add *")
  commit_msg <- paste0('"Pushed from ', build_link, '"')
  system(paste('git commit -m', commit_msg))
  # This post explains how this works -- http://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html
  repo <- sprintf("https://%s@github.com/cpsievert/plotly-test-table.git", Sys.getenv("GH_TOKEN"))
  system(paste("git pull", repo, "gh-pages"))
  system(paste("git push", repo, "gh-pages"))
}
