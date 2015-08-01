context("legends")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("legend-", name))
  L <- gg2list(gg)
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, layout=L$layout)
}

test_that("legend can be hidden", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width, color=Species)) +
    theme(legend.position="none")
  info <- expect_traces(ggiris, 3, "iris-position-none")
  expect_identical(info$layout$showlegend, FALSE)
})

getnames <- function(traces){
  name.list <- lapply(traces, "[[", "name")
  ## Not sapply, since that will result in a character vector with
  ## "NULL" if one of the traces does not have an element "name"
  do.call(c, name.list)
}

test_that("legend entries appear in the correct order", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width, color=Species))
  info <- expect_traces(ggiris, 3, "iris-default")
  computed.showlegend <- sapply(info$traces, "[[", "showlegend")
  expected.showlegend <- rep(TRUE, 3)
  expect_identical(as.logical(computed.showlegend), expected.showlegend)
  ## Default is the same as factor levels.
  expect_identical(getnames(info$traces), levels(iris$Species))
  ## Custom breaks should be respected.
  breaks <- c("versicolor", "setosa", "virginica")
  ggbreaks <- ggiris+scale_color_discrete(breaks=breaks)
  info.breaks <- expect_traces(ggbreaks, 3, "iris-breaks")
  expect_identical(getnames(info.breaks$traces), breaks)
})

test_that("2 breaks -> 1 named trace with showlegend=FALSE", {
  two.breaks <- c("setosa", "versicolor")
  two.legend.entries <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width, color=Species)) +
    scale_color_discrete(breaks=two.breaks)
  info <- expect_traces(two.legend.entries, 3, "iris-trace-showlegend-FALSE")
  expected.names <- levels(iris$Species)
  expected.showlegend <- expected.names %in% two.breaks
  expect_identical(getnames(info$traces), expected.names)
  computed.showlegend <- sapply(info$traces, "[[", "showlegend")
  expect_identical(as.logical(computed.showlegend), expected.showlegend)
})

test_that("1 break -> 2 traces with showlegend=FALSE", {
  one.break <- c("setosa")
  one.legend.entry <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width, color=Species)) +
    scale_color_discrete(breaks=one.break)
  info <- expect_traces(one.legend.entry, 3, "iris-2traces-showlegend-FALSE")
  expected.names <- levels(iris$Species)
  expected.showlegend <- expected.names %in% one.break
  expect_identical(getnames(info$traces), expected.names)
  computed.showlegend <- sapply(info$traces, "[[", "showlegend")
  expect_identical(as.logical(computed.showlegend), expected.showlegend)
})

test_that("0 breaks -> 3 traces with showlegend=FALSE", {
  no.breaks <- c()
  no.legend.entries <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width, color=Species)) +
    scale_color_discrete(breaks=no.breaks)
  info <- expect_traces(no.legend.entries, 3, "iris-3traces-showlegend-FALSE")
  expect_equal(length(info$layout$annotations), 0)
  expected.names <- levels(iris$Species)
  expected.showlegend <- expected.names %in% no.breaks
  expect_identical(getnames(info$traces), expected.names)
  computed.showlegend <- sapply(info$traces, "[[", "showlegend")
  expect_identical(as.logical(computed.showlegend), expected.showlegend)
})

# test of legend position
test_that("very long legend items", {
  long_items <- data.frame(cat1 = sample(x = LETTERS[1:10], 
                                         size = 100, replace = TRUE),
                           cat2 = sample(x = c("AAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
                                               "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
                                               "CCCCCCCCCCCCCCCCCCCCCCCCCCCCC"),
                                         size = 100, replace = TRUE))
  p_long_items <- ggplot(long_items, aes(cat1, fill=cat2)) + 
    geom_bar(position="dodge")
  info <- expect_traces(p_long_items, 3, "very long legend items")
  expect_equal(length(info$layout$annotations), 1)
  expected.names <- levels(long_items$cat2)
  expect_identical(info$layout$annotations[[1]]$y - 
                     info$layout$legend$y > 0, TRUE)
})

# test of legend position
test_that("many legend items", {
  many_items <- data.frame(cat1 = sample(x = paste0("Group ", LETTERS[1:12]), 
                                         size = 100, replace = TRUE),
                           cat2 = sample(x = c("foo", "bar", "baz"),
                                         size = 100, replace = TRUE))
  p_many_items <- ggplot(many_items, aes(cat2, fill=cat1)) + 
    geom_bar(position="dodge")
  info <- expect_traces(p_many_items, 12, "many legend items")
  expect_equal(length(info$layout$annotations), 1)
  expected.names <- levels(many_items$cat2)
  expect_identical(info$layout$annotations[[1]]$y > 0.5, TRUE)
  expect_identical(info$layout$annotations[[1]]$y - 
                     info$layout$legend$y > 0, TRUE)
})

