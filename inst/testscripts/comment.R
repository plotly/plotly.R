# first argument should be the pull request number
# second should be SHA-1 from the relevant commit
# third should be the authentication token
a <- commandArgs(TRUE)
# post a comment on the pull request with link to new test table
# https://developer.github.com/v3/pulls/comments/#create-a-comment
# note that gistr is a good reference for talking to the github API via httr
# https://github.com/ropensci/gistr/blob/master/R/zzz.R
library("httr")
url <- sprintf('https://api.github.com/repos/ropensci/plotly/issues/%s/comments', a[1])
tbl_link <- sprintf("http://ropensci.github.io/plotly-test-table/tables/%s/index.html", a[2])
header <- add_headers(`User-Agent` = "plotly", 
                      `Accept` = 'application/vnd.github.v3+json',
                      `Authorization` = paste0("token ", a[3]))
res <- content(GET(url = url, header))
old_body <- unlist(lapply(res, "[[", "body"))
# post comment if a link to this SHA doesn't exist 
# (needed since Travis randomly re-builds stuff)
if (!any(grepl(tbl_link, old_body))) {
  msg <- sprintf("New test table created at %s", tbl_link)
  json <- jsonlite::toJSON(list(body = msg), auto_unbox = TRUE)
  httr::POST(url = url, header, body = json, encode = "json")
} else {
  message("Link already posted")
}
