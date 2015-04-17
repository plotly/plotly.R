context("Probability density")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("density-", name))
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

# Draw a probability density estimation using geom_density
base <- ggplot(mtcars, aes(wt))

test_that("geom_density() is translated to area chart", {
  info <- expect_traces(base + geom_density(), 1, "simple")
  tr <- info$traces[[1]]
  expect_identical(tr$type, "scatter")
  expect_identical(tr$fill, "tozeroy")
  expect_identical(tr$fillcolor, "rgba(51,51,51,0)")
})

test_that("geom_density() respects fill aesthetic", {
  info <- expect_traces(base + geom_density(aes(fill=factor(vs))), 2, "fill")
  trs <- info$traces
  type <- unique(sapply(trs, "[[", "type"))
  fill <- unique(sapply(trs, "[[", "fill"))
  expect_identical(type, "scatter")
  expect_identical(fill, "tozeroy")
})

test_that("geom_density() respects colour aesthetic", {
  info <- expect_traces(base + geom_density(aes(colour=factor(vs))), 2, "color")
  trs <- info$traces
  type <- unique(sapply(trs, "[[", "type"))
  fill <- unique(sapply(trs, "[[", "fill"))
  expect_identical(type, "scatter")
  expect_identical(fill, "tozeroy")
})
