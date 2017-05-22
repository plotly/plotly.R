context("legends")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("legend-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

p <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(vs), shape = factor(cyl))) + 
  geom_point()

test_that("Discrete colour and shape get merged into one legend", {
  info <- save_outputs(p, "scatter_legend")
  expect_equivalent(length(info$data), 5)
  expect_true(info$layout$showlegend)
  # 5 legend entries
  expect_equivalent(sum(sapply(info$data, "[[", "showlegend")), 5)
  # verify entries are sorted correctly
  nms <- sapply(info$data, "[[", "name")
  d <- unique(mtcars[c("vs", "cyl")])
  d <- d[order(d$vs, d$cyl), ]
  expect_identical(
    nms, paste0("(", d$vs, ",", d$cyl, ")")
  )
  a <- info$layout$annotations
  expect_match(a[[1]]$text, "^factor\\(vs\\)")
  expect_match(a[[1]]$text, "factor\\(cyl\\)$")
  expect_true(a[[1]]$y > info$layout$legend$y)
})


test_that("legend vanishes when theme(legend.position = 'none'')", {
  info <- expect_traces(p + theme(legend.position = "none"), 5, "hide")
  expect_identical(info$layout$showlegend, FALSE)
})

p <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(vs))) + 
  geom_point()

# TODO: better support for scale_*_discrete()
#test_that("trace order respects scale_color_discrete()", {
#  g <- p + scale_color_discrete(breaks = c(1, 0))
#  info <- expect_traces(g, 2, "iris-default")
#  nms <- unlist(lapply(info$data, "[[", "name"))
#  expect_true(all(nms == c("factor(vs): 1", "factor(vs): 0")))
#})
#
#test_that("missing breaks translates to showlegend=FALSE", {
#  g <- p + scale_color_discrete(breaks = 1)
#  info <- expect_traces(two.legend.entries, 3, "iris-trace-showlegend-FALSE")
#  expect_equivalent(sum(sapply(info, "[[", "showlegend")), 1)
#})

# test of legend position
test_that("very long legend items", {
  long_items <- data.frame(
    cat1 = sample(x = LETTERS[1:10], 
                  size = 100, replace = TRUE),
    cat2 = sample(x = c("AAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
                        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
                        "CCCCCCCCCCCCCCCCCCCCCCCCCCCCC"),
                  size = 100, replace = TRUE)
  )
  p_long_items <- ggplot(long_items, aes(cat1, fill = cat2)) + 
    geom_bar(position = "dodge")
  info <- expect_traces(p_long_items, 3, "very-long-legend-items")
})

iris$All <- "All species"
p <- qplot(data = iris, x = Sepal.Length, y = Sepal.Width, color = All)

test_that("legend is created with discrete mapping regardless of unique values", {
  info <- expect_traces(p, 1, "one-entry")
  expect_true(info$data[[1]]$showlegend)
  expect_true(info$layout$showlegend)
  expect_equivalent(length(info$layout$annotations), 1)
})

test_that("can hide legend", {
  info <- expect_traces(hide_legend(p), 1, "hide-legend")
  expect_false(info$layout$showlegend)
  expect_null(info$layout$annotations %||% NULL)
})


# test of legend position
test_that("many legend items", {
  p <- ggplot(midwest, aes(category, fill = category)) + geom_bar()
  info <- expect_traces(p, length(unique(midwest$category)), "many legend items")
})

