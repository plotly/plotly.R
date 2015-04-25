# first argument should be the pull request number
# second should be SHA-1 from the relevant commit
a <- commandArgs(TRUE)
# post a comment on the pull request with link to new test table
# https://developer.github.com/v3/pulls/comments/#create-a-comment
# note that gistr is a good reference for talking to the github API via httr
# https://github.com/ropensci/gistr/blob/master/R/zzz.R
url <- sprintf('https://api.github.com/repos/ropensci/plotly/pulls/%s/comments', a[1])
home <- sprintf("http://ropensci.github.io/plotly-test-table/tables/%s/index.html", a[2])
header <- add_headers(`User-Agent` = "plotly", `Accept` = 'application/vnd.github.v3+json')
msg <- sprintf("New test table created at \n '%s'", new_test)
httr::POST(url = url, header, body = list(body = msg), encode = "json")
