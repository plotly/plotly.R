# first argument should be the pull request number
# third should be the authentication token
a <- commandArgs(TRUE)
# gistr is a good reference for talking to the github API via httr
# https://github.com/ropensci/gistr/blob/master/R/zzz.R
library("httr")
base <- 'https://api.github.com/repos/ropensci/plotly/'
pr <- sprintf(file.path(base, 'pulls/%s/'), a[1])
comments <- sprintf(file.path(base, 'issues/%s/comments'), a[1])
header <- add_headers(`User-Agent` = "plotly", 
                      `Accept` = 'application/vnd.github.v3+json',
                      `Authorization` = paste0("token ", a[3]))
# Must be successful since we grab the branch name for this pull request
# and SHA1 info from the request content
res <- GET(url = pr, header)
stop_for_status(res)
info <- content(res)
# find the branch name for this pull request
# http://stackoverflow.com/questions/15096331/github-api-how-to-find-the-branches-of-a-pull-request
branch <- strsplit(info$head$label, ":")[[1]][2]

# plotly-test-table build script assumes we've checkout the dev branch
setwd("../plotly")
if (system(paste("git checkout", branch)) != 0L)
  stop(paste("Failed to 'git checkout'", branch, "branch"))
setwd("../plotly-test-table")
cat("user,SHA1,label", file = "code_commits.csv")
row1 <- paste0("\nropensci,", info$base$sha, ",master")
cat(row1, file = "code_commits.csv", append = TRUE)
row2 <- paste0("\nropensci,", info$head$sha, ",", branch)
cat(row2, file = "code_commits.csv", append = TRUE)

# copy over file (created during Rscript) 
# with sha/branch info for building test table
system("touch table.R")
if (system("make") != 0L)
  stop("Failed to 'make' test table")

# post comment if a link to this SHA doesn't exist 
# (needed since Travis randomly re-builds stuff)
tbl_link <- sprintf("http://ropensci.github.io/plotly-test-table/tables/%s/index.html", 
                    info$head$sha)
res <- GET(url = comments, header)
warn_for_status(res)
info <- content(res)
old_body <- unlist(lapply(info, "[", "body"))
if (!any(grepl(tbl_link, old_body))) {
  msg <- sprintf("New test table created at %s", tbl_link)
  json <- jsonlite::toJSON(list(body = msg), auto_unbox = TRUE)
  httr::POST(url = url, header, body = json, encode = "json")
} else {
  message("Link already posted")
}
