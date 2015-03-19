context("smooth")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("smooth-", name))
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

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + geom_smooth()

test_that("geom_point() + geom_smooth() produces 3 traces", {
  expect_traces(p, 3, "basic")
})

p2 <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + 
  geom_smooth(se = FALSE)

test_that("geom_point() + geom_smooth(se = FALSE) produces 2 traces", {
  expect_traces(p2, 2, "se-false")
})

d <- diamonds[sample(nrow(diamonds), 1000), ]
p3 <- qplot(carat, price, group = cut, data = d) + geom_smooth()

test_that("geom_smooth() respects group aesthetic", {
  info <- expect_traces(p3, 3, "group")
})

p4 <- qplot(carat, price, colour = cut, data = d) + geom_smooth()
p5 <- qplot(carat, price, data = d) + geom_smooth(aes(colour = cut))

test_that("geom_smooth() respects colour aesthetic", {
  info <- expect_traces(p4, 11, "colour")
  # number of showlegends should equal the number of factor levels 
  n <- sum(unlist(sapply(info$traces, "[[", "showlegend")))
  expect_equal(n, nlevels(d$cut))
  info <- expect_traces(p5, 7, "colour2")
  n <- sum(unlist(sapply(info$traces, "[[", "showlegend")))
  expect_equal(n, nlevels(d$cut))
})

p7 <- qplot(carat, price, data = d) + geom_smooth(aes(fill = cut))

test_that("geom_smooth() respects fill aesthetic", {
  info <- expect_traces(p7, 7, "fill2")
  n <- sum(unlist(sapply(info$traces, "[[", "showlegend")))
  expect_equal(n, nlevels(d$cut))
})

# ensure legend is drawn when needed
p8 <- qplot(carat, price, data = d) + facet_wrap(~cut) + 
  geom_smooth(aes(colour = cut, fill = cut))

test_that("geom_smooth() works with facets", {
  # 3 traces for each panel
  info <- expect_traces(p8, 15, "fill2")
})

