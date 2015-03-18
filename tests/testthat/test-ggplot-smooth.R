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

d <- diamonds[sample(nrows(diamonds, 1000)), ]
p3 <- qplot(carat, price, group = cut, data = d) + geom_smooth()

test_that("geom_smooth() respects group aesthetic", {
  # 1 trace for points
  # 5 traces for lines (1 for each group)
  # 5 traces for ribbons (1 for each group)
  expect_traces(p3, 11, "group")
})

p4 <- qplot(carat, price, colour = cut, data = d) + geom_smooth()

test_that("geom_smooth() respects colour aesthetic", {
  expect_traces(p4, 11, "colour")
})

p5 <- qplot(carat, price, fill = cut, data = d) + geom_smooth()

test_that("geom_smooth() respects fill aesthetic", {
  expect_traces(p5, 11, "fill")
})
