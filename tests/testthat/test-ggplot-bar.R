context("bar")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("bar-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

researchers <- data.frame(
  country = c("Canada", "Canada", "Germany", "USA"),
  name = c("Warren", "Andreanne", "Stefan", "Toby"),
  papers = c(23, 14, 37, 20),
  field = c("Math", "Bio", "Bio", "Math")
)

gg <- ggplot(researchers, aes(country, papers, fill = field))

test_that("position_dodge is translated to barmode=group", {
  gg.dodge <- gg + geom_bar(stat = "identity", position = "dodge")
  info <- expect_traces(gg.dodge, 2, "dodge")
  trs <- info$data
  trace.names <- sapply(trs[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(info$layout$barmode, "group")
  # Check x values
  expect_identical(as.character(trs[[1]]$x), c("Canada", "Germany"))
  expect_identical(as.character(trs[[2]]$x), c("Canada", "USA"))
})

test_that("position_stack is translated to barmode=stack", {
  gg.stack <- gg + geom_bar(stat="identity", position="stack")
  info <- expect_traces(gg.stack, 2, "stack")
  trs <- info$data
  trace.names <- sapply(trs[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(info$layout$barmode, "stack")
})

test_that("position_identity is translated to barmode=stack", {
  gg.identity <- gg + geom_bar(stat="identity", position="identity")
  info <- expect_traces(gg.identity, 2, "identity")
  trs <- info$data
  trace.names <- sapply(trs[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(info$layout$barmode, "stack")
})

test_that("dates work well with bar charts", {
  researchers$month <- c("2012-01-01", "2012-01-01", "2012-02-01", "2012-02-01")
  researchers$month <- as.Date(researchers$month)
  gd <- ggplot(researchers, aes(month, papers, fill = field)) +
    geom_bar(stat = "identity")
  info <- expect_traces(gd, 2, "dates")
  trs <- info$data
  expect_identical(info$layout$xaxis$type, "date")
  # plotly likes time in milliseconds
  t <- as.numeric(unique(researchers$month)) * 24 * 60 * 60 * 1000
  expect_equal(trs[[1]]$x, t)
})

## http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_%28ggplot2%29/
df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
                 total_bill = c(14.89, 17.23))

test_that("Very basic bar graph", {
  gg <- ggplot(data = df, aes(x = time, y = total_bill)) +
    geom_bar(stat = "identity")
  info <- expect_traces(gg, 1, "nocolor")
  tr <- info$data[[1]]
  expect_identical(tr$type, "bar")
  expect_identical(tr$bargap, "default")
  expect_identical(tr$x, df$time)
  expect_identical(tr$y, df$total_bill)
  expect_null(info$layout$annotations)
  expect_false(info$layout$showlegend)
})

test_that("Map the time of day to different fill colors", {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, fill = time)) +
    geom_bar(stat = "identity")
  info <- expect_traces(gg, 2, "color")
  # is the color of the two bars the same?
  same <- identical(info$data[[1]]$marker$color, info$data[[2]]$marker$color)
  expect_true(!same)
  expect_match(info$layout$annotations[[1]]$text, "time")
  expect_true(info$layout$showlegend)
})

test_that("Add a black outline", {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, fill = time)) +
    geom_bar(colour = "black", stat = "identity")
  info <- expect_traces(gg, 2, "black-outline")
  for(tr in info$data){
    expect_true(is.character(tr$marker$color))
    expect_identical(tr$marker$line$color, toRGB("black"))
    expect_true(tr$showlegend)
  }
  expect_match(info$layout$annotations[[1]]$text, "time")
  expect_true(info$layout$showlegend)
})

test_that("guides(fill=FALSE) hides fill legend", {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, fill = time)) +
    geom_bar(colour = "black", stat = "identity") +
    guides(fill = FALSE)
  info <- expect_traces(gg, 2, "aes-fill-guides-fill-FALSE")
  for(tr in info$data){
    expect_true(is.character(tr$marker$color))
    expect_identical(tr$marker$line$color, toRGB("black"))
  }
  expect_null(info$layout$annotations)
  expect_false(info$layout$showlegend)
})

test_that('guides(fill="none") hides fill legend', {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, fill = time)) +
    geom_bar(colour = "black", stat = "identity") +
    guides(fill = "none")
  info <- expect_traces(gg, 2, "aes-fill-guides-fill-none")
  expect_null(info$layout$annotations)
  expect_false(info$layout$showlegend)
})

test_that('guides(colour="none") does not affect fill legend', {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, fill = time)) +
    geom_bar(color = "black", stat = "identity") +
    guides(colour = "none")
  info <- expect_traces(gg, 2, "aes-fill-guides-color-none")
  expect_match(info$layout$annotations[[1]]$text, "time")
  expect_true(info$layout$showlegend)
})

test_that("guides(fill=FALSE) does not affect colour legend", {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, colour = time)) +
    geom_bar(fill = "grey", stat = "identity") +
    guides(fill = FALSE)
  info <- expect_traces(gg, 2, "aes-colour-guides-fill-FALSE")
  for(tr in info$data){
    expect_identical(tr$marker$color, toRGB("grey"))
    expect_true(is.character(tr$marker$line$color))
    expect_true(tr$showlegend)
  }
  expect_match(info$layout$annotations[[1]]$text, "time")
  expect_true(info$layout$showlegend)
})


base <- ggplot(mtcars, aes(factor(vs), fill = factor(cyl))) 

test_that("geom_bar() stacks counts", { 
  info <- expect_traces(base + geom_bar(), 3, "position-stack")
  expect_identical(info$layout$barmode, "stack")
  trs <- info$data
  # sum of y values for each trace 
  test <- as.numeric(sort(sapply(trs, function(x) sum(x$y))))
  true <- as.numeric(sort(table(mtcars$cyl)))
  expect_identical(test, true)
})

test_that("geom_bar(position = 'fill') stacks proportions", {
  info <- expect_traces(base + geom_bar(position = "fill"), 3, "position-fill")
  expect_identical(info$layout$barmode, "stack")
  trs <- info$data
  # sum of y-values *conditioned* on a x-value
  prop <- sum(sapply(sapply(trs, "[[", "y"), "[", 1))
  expect_identical(prop, 1)
})

d <- head(diamonds, 50)
gbar <- ggplot(d, aes(cut, price)) + geom_bar(stat = "identity")

test_that("For a given x value, if multiple y exist, sum them. ", {
  info <- expect_traces(gbar, 1, "category-names")
  expect_identical(info$data[[1]]$type, "bar")
  y <- with(d, tapply(price, cut, sum))
  # make sure order of counts match
  y <- y[info$data[[1]]$x]
  expect_equal(info$data[[1]]$y, as.numeric(y))
})
