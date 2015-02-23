context("bar")

researchers <-
  data.frame(country=c("Canada", "Canada", "Germany", "USA"),
             name=c("Warren", "Andreanne", "Stefan", "Toby"),
             papers=c(23, 14, 37, 20),
             field=c("Math", "Bio", "Bio", "Math"))

gg <- ggplot(researchers, aes(country, papers, fill=field))

test_that("position_dodge is translated to barmode=group", {
  gg.dodge <- gg + geom_bar(stat="identity", position="dodge")
  L <- gg2list(gg.dodge)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "group")
  # Check x values
  expect_identical(as.character(L[[1]]$x[1]), "Canada")
  expect_identical(as.character(L[[1]]$x[2]), "Germany")
  expect_identical(as.character(L[[2]]$x[1]), "Canada")
  expect_identical(as.character(L[[2]]$x[2]), "USA")
  
  save_outputs(gg.dodge, "bar-dodge")
})

test_that("position_stack is translated to barmode=stack", {
  gg.stack <- gg + geom_bar(stat="identity", position="stack")
  L <- gg2list(gg.stack)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "stack")
  
  save_outputs(gg.stack, "bar-stack")
})

test_that("position_identity is translated to barmode=overlay", {
  gg.identity <- gg + geom_bar(stat="identity", position="identity")
  L <- gg2list(gg.identity)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "overlay")
  
  save_outputs(gg.identity, "bar-identity")
})

test_that("dates work well with bar charts", {
  
  researchers$month <- c("2012-01-01", "2012-01-01", "2012-02-01", "2012-02-01")
  researchers$month <- as.Date(researchers$month)
  
  gd <- ggplot(researchers, aes(month, papers, fill=field)) +
    geom_bar(stat="identity")
  
  L <- gg2list(gd)
  
  expect_equal(length(L), 3)  # 2 traces + layout
  expect_identical(L$kwargs$layout$xaxis$type, "date")
  expect_identical(L[[1]]$x[1], "2012-01-01 00:00:00")
  expect_identical(L[[1]]$x[2], "2012-02-01 00:00:00")
  
  save_outputs(gd, "bar-dates")
})

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("bar-", name))
  L <- gg2list(gg)
  is.trace <- names(L) == ""
  all.traces <- L[is.trace]
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, kwargs=L$kwargs)
}

## http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_%28ggplot2%29/
df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
                 total_bill = c(14.89, 17.23))

test_that("Very basic bar graph", {
  gg <- ggplot(data=df, aes(x=time, y=total_bill)) +
    geom_bar(stat="identity")
  info <- expect_traces(gg, 1, "nocolor")
  for(tr in info$traces){
    expect_null(tr$marker$color)
    expect_null(tr$marker$line$color)
    expect_null(tr$marker$line$width)
    expect_null(info$kwargs$layout$annotations)
    expect_false(info$kwargs$layout$showlegend)
  }
})

test_that("Map the time of day to different fill colors", {
  gg <- ggplot(data=df, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(stat="identity")
  info <- expect_traces(gg, 2, "color")
  for(tr in info$traces){
    expect_true(is.character(tr$marker$color))
    expect_null(tr$marker$line$color)
    expect_null(tr$marker$line$width)
    expect_match(info$kwargs$layout$annotations[[1]]$text, "time")
    expect_true(info$kwargs$layout$showlegend)
  }
})

test_that("Add a black outline", {
  gg <- ggplot(data=df, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(colour="black", stat="identity")
  info <- expect_traces(gg, 2, "black-outline")
  for(tr in info$traces){
    expect_true(is.character(tr$marker$color))
    expect_identical(tr$marker$line$color, toRGB("black"))
    expect_equal(tr$marker$line$width, 1)
    expect_match(info$kwargs$layout$annotations[[1]]$text, "time")
    expect_true(info$kwargs$layout$showlegend)
  }
})

test_that("No legend, since the information is redundant", {
  gg <- ggplot(data=df, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE)
  info <- expect_traces(gg, 2, "black-outline")
  for(tr in info$traces){
    expect_true(is.character(tr$marker$color))
    expect_identical(tr$marker$line$color, toRGB("black"))
    expect_equal(tr$marker$line$width, 1)
    expect_null(info$kwargs$layout$annotations)
    expect_false(info$kwargs$layout$showlegend)
  }
})
