context("bar")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("bar-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

researchers <- data.frame(
  country = c("Canada", "Canada", "Germany", "USA"),
  name = c("Warren", "Andreanne", "Stefan", "Toby"),
  papers = c(23, 14, 37, 20),
  field = c("Math", "Bio", "Bio", "Math")
)

gg <- ggplot(researchers, aes(country, papers, fill = field))

test_that("position_dodge()", {
  gg.dodge <- gg + geom_bar(stat = "identity", position = "dodge")
  info <- expect_traces(gg.dodge, 2, "dodge")
  expect_identical(info$layout$barmode, "relative")
  
  l <- ggplotly(gg.dodge, dynamicTicks = "x")$x
  expect_identical(l$layout$barmode, "dodge")
  expect_equivalent(l$data[[1]]$x, c("Canada", "Germany"))
  expect_equivalent(l$data[[1]]$name, "Bio")
  expect_equivalent(l$data[[2]]$x, c("Canada", "USA"))
  expect_equivalent(l$data[[2]]$name, "Math")
})

test_that("position_stack()", {
  gg.stack <- gg + geom_bar(stat = "identity", position = "stack")
  info <- expect_traces(gg.stack, 2, "stack")
  expect_identical(info$layout$barmode, "relative")
  
  l <- ggplotly(gg.stack, dynamicTicks = T)$x
  expect_identical(l$layout$barmode, "relative")
})

test_that("position_identity()", {
  gg.identity <- gg + geom_bar(stat = "identity", position = "identity")
  info <- expect_traces(gg.identity, 2, "identity")
  expect_identical(info$layout$barmode, "relative")
  
  l <- ggplotly(gg.identity, dynamicTicks = T)$x
  expect_identical(l$layout$barmode, "relative")
})

test_that("dates work well with bar charts", {
  researchers$month <- c("2012-01-01", "2012-01-01", "2012-02-01", "2012-02-01")
  researchers$month <- as.Date(researchers$month)
  gd <- ggplot(researchers, aes(month, papers, fill = field)) +
    geom_bar(stat = "identity")
  info <- expect_traces(gd, 2, "dates")
  
  # by default, date axes are linear...
  expect_equivalent(info$layout$xaxis$type, "linear")
  expect_equivalent(
    info$data[[1]]$x,
    as.numeric(unique(researchers$month))
  )
  
  # different story for dynamicTicks...
  l <- ggplotly(gd, dynamicTicks = TRUE)$x
  expect_equivalent(l$layout$xaxis$type, "date")
  expect_equivalent(l$layout$xaxis$tickmode, "auto")
  expect_is(l$layout$xaxis$range, "Date")
  for (attr in c("x", "width")) {
    expect_is(l$data[[1]][[attr]], "Date")
  }
  
})

## http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_%28ggplot2%29/
df <- data.frame(
  time = factor(c("Lunch","Dinner"), levels = c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)

test_that("Very basic bar graph", {
  gg <- ggplot(data = df, aes(x = time, y = total_bill)) +
    geom_bar(stat = "identity")
  info <- expect_traces(gg, 1, "nocolor")
  tr <- info$data[[1]]
  expect_identical(tr$type, "bar")
  expect_equivalent(tr$y, df$total_bill)
})

test_that("Map the time of day to different fill colors", {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, fill = time)) +
    geom_bar(stat = "identity")
  info <- expect_traces(gg, 2, "color")
  # is the color of the two bars the same?
  expect_false(
    identical(info$data[[1]]$marker$color, info$data[[2]]$marker$color)
  )
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
  expect_true(info$layout$showlegend)
})


test_that('guides(colour="none") does not affect fill legend', {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, fill = time)) +
    geom_bar(color = "black", stat = "identity") +
    guides(colour = "none")
  info <- expect_traces(gg, 2, "aes-fill-guides-color-none")
  expect_true(info$layout$showlegend)
})

test_that("guides(fill=FALSE) does not affect colour legend", {
  gg <- ggplot(data = df, aes(x = time, y = total_bill, colour = time)) +
    geom_bar(fill = "grey", stat = "identity") +
    guides(fill = FALSE)
  info <- expect_traces(gg, 2, "aes-colour-guides-fill-FALSE")
  for(tr in info$data){
    expect_equivalent(tr$marker$color, toRGB("grey"))
    expect_true(is.character(tr$marker$line$color))
    expect_true(tr$showlegend)
  }
  expect_match(info$layout$annotations[[1]]$text, "time")
  expect_true(info$layout$showlegend)
})


base <- ggplot(mtcars, aes(factor(vs), fill = factor(cyl))) 

test_that("geom_bar() stacks counts", { 
  info <- expect_traces(base + geom_bar(), 3, "position-stack")
  expect_identical(info$layout$barmode, "relative")
  trs <- info$data
  # sum of y values for each trace 
  test <- as.numeric(sort(sapply(trs, function(x) sum(x$y))))
  true <- as.numeric(sort(table(mtcars$cyl)))
  expect_identical(test, true)
})

test_that("geom_bar(position = 'fill') stacks proportions", {
  info <- expect_traces(base + geom_bar(position = "fill"), 3, "position-fill")
  expect_identical(info$layout$barmode, "relative")
  trs <- info$data
  # sum of y-values *conditioned* on a x-value
  prop <- sum(sapply(sapply(trs, "[[", "y"), "[", 1))
  expect_identical(prop, 1)
})

d <- diamonds[1:50, ]
gbar <- ggplot(d, aes(cut, price)) + geom_bar(stat = "identity")

test_that("Using identity with multiple y for a given x works ", {
  info <- expect_traces(gbar, 1, "category-names")
})

p <- ggplot(mtcars, aes(factor(cyl))) + geom_bar() + coord_flip()

test_that("geom_bar() + coord_flip() works", {
  info <- expect_traces(p, 1, "coord-flip")
  expect_identical(info$data[[1]]$orientation, "h")
})

