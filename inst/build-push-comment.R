# -----------------------------------------------------------------------
# Travis does two types of builds:
#
# (1) A so-called "push". This essentially does a checkout on the most
# recent commit of the pull request, but *doesn't* merge with master.
# In this case, $TRAVIS_PULL_REQUEST = "false"
# (2) A so-called "pr" (pull request). This *simulates* a merge with master.
# In this case, $TRAVIS_PULL_REQUEST contains the pull request number.
# And $TRAVIS_COMMIT is a SHA1 hash that won't match the HEAD of master or
# the branch we're testing.
#
# Since it makes more sense to visually compared what we'd see *after* we
# merge with master, we don't do anything here if it's a push build.
#
# For more info on Travis CI environment variables, see 
# http://docs.travis-ci.com/user/ci-environment/#Environment-variables
# -----------------------------------------------------------------------
library("httr")
library("testthat")

# if this is a Travis pull request, make a comment on the pull request
comment <- grepl("^[0-9]+$", Sys.getenv("TRAVIS_PULL_REQUEST"))

if (comment) {
  # get the links to differences, if any
  this_hash <- substr(Sys.getenv("TRAVIS_COMMIT"), 1, 7)
  this_dir <- normalizePath(this_hash, mustWork = T)
  links <- sprintf(
    "http://cpsievert.github.io/plotly-test-table/%s/%s", 
    this_hash, dir(this_dir)
  )
  # construct the message 
  build_link <- file.path('https://travis-ci.org/ropensci/plotly/builds',
                          Sys.getenv("TRAVIS_BUILD_ID"))
  msg <- sprintf(
    "> The message below was automatically generated after build %s \n\n Detected %s differences -> \n\n %s", 
    build_link, length(links), paste(links, collapse = "\n")
  )
  # gistr is a good reference for talking to the github API via httr
  # https://github.com/ropensci/gistr/blob/master/R/zzz.R
  base <- 'https://api.github.com/repos/ropensci/plotly/'
  header <- add_headers(
    `User-Agent` = "plotly",
    `Accept` = 'application/vnd.github.v3+json',
    `Authorization` = paste0("token ", Sys.getenv("GITHUB_PAT"))
  )
  commentz <- sprintf(
    paste0(base, 'issues/%s/comments'), 
    Sys.getenv("TRAVIS_PULL_REQUEST")
  )
  res <- GET(commentz, header)
  warn_for_status(res)
  old_body <- unlist(lapply(content(res), "[", "body"))
  # only post a comment if this hash doesn't appear in any of the comments
  # (needed since Travis sometimes randomly re-builds stuff)
  if (!any(grepl(this_hash, old_body))) {
    json <- jsonlite::toJSON(list(body = msg), auto_unbox = TRUE)
    POST(url = commentz, header, body = json, encode = "json")
  } else {
    message("Link already posted")
  }
  # update plotly-test-table
  system("git status")
  system("git add *")
  system(sprintf("git commit -q -m 'Pushed from %s'", build_link))
  # This post explains how this works -- http://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html
  repo <- sprintf(
    "https://%s@github.com/cpsievert/plotly-test-table.git", 
    Sys.getenv("GITHUB_PAT")
  )
  system(paste("git pull", repo, "gh-pages"))
  system(paste("git push -q", repo, "gh-pages"))
}
