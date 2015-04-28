# first argument is the pull request number (TRAVIS_PULL_REQUEST)
# second is travis build ID (TRAVIS_BUILD_ID)
# third is the commit SHA1 currently being tested (TRAVIS_COMMIT)
# fourth is the github authentication token 
a <- commandArgs(TRUE)
# gistr is a good reference for talking to the github API via httr
# https://github.com/ropensci/gistr/blob/master/R/zzz.R
library("httr")
base <- 'https://api.github.com/repos/ropensci/plotly/'
pr <- sprintf(paste0(base, 'pulls/%s'), a[1])
header <- add_headers(`User-Agent` = "plotly", 
                      `Accept` = 'application/vnd.github.v3+json',
                      `Authorization` = paste0("token ", a[4]))
# Must be successful since we grab the branch name for this pull request
# and SHA1 info from the request content
res <- GET(url = pr, header)
stop_for_status(res)
info <- content(res)
# find the branch name for this pull request
# http://stackoverflow.com/questions/15096331/github-api-how-to-find-the-branches-of-a-pull-request
branch <- strsplit(info$head$label, ":")[[1]][2]

# plotly-test-table build script assumes we've checkout the dev branch.
# Note that travis does something like this for "pr" build:
#$ git fetch origin +refs/pull/number/merge:
#$ git checkout -qf FETCH_HEAD
# this leaves HEAD in a detached state, but we should be able to do:
# git checkout -b new_branch_name
setwd("../plotly")
if (system(paste("git checkout -b", branch)) != 0L)
  stop(paste("Failed to 'git checkout -b'", branch, "branch"))
devtools::install()
setwd("../plotly-test-table")
cat("user,SHA1,label", file = "code_commits.csv")
row1 <- paste0("\nropensci,", info$base$sha, ",master")
cat(row1, file = "code_commits.csv", append = TRUE)
row2 <- paste0("\nropensci,", a[3], ",", branch)
cat(row2, file = "code_commits.csv", append = TRUE)

# copy over file (created during Rscript) 
# with sha/branch info for building test table
system("touch table.R")
if (system("make") != 0L) stop("Failed to 'make' test table")

# add, commit, push to gh-pages branch of plotly-test-table
system("git add index.html")
system("git add tables/*/*.html")
system("git add data/*/*.png")
system("git add data/*/*.log")
commit_msg <- paste0('"Pushed from https://travis-ci.org/ropensci/plotly/builds/"', a[2])
system(paste('git commit -m', commit_msg))
# This post explains how this works -- http://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html
repo <- sprintf("https://%s@github.com/ropensci/plotly-test-table.git", a[4])
system(paste("git pull -q", repo, "gh-pages"))
system(paste("git push -q", repo, "gh-pages"))

# post comment if a link to this SHA doesn't exist 
# (needed since Travis randomly re-builds stuff)
tbl_link <- sprintf("http://ropensci.github.io/plotly-test-table/tables/%s/index.html", a[3])
msg <- sprintf("On TravisCI, commit %s was successfully merged with %s (master) to create %s. A visual testing table comparing %s with %s can be found here:\n %s", 
               info$head$sha, info$base$sha, a[3], info$base$sha, a[3], tbl_link)
msg <- paste0("```---Automatically generated message---```", msg, "```---Automatically generated message---```")
commentz <- sprintf(paste0(base, 'issues/%s/comments'), a[1])
res <- GET(commentz, header)
warn_for_status(res)
info <- content(res)
old_body <- unlist(lapply(info, "[", "body"))
if (!any(grepl(tbl_link, old_body))) {
  json <- jsonlite::toJSON(list(body = msg), auto_unbox = TRUE)
  httr::POST(url = commentz, header, body = json, encode = "json")
} else {
  message("Link already posted")
}
