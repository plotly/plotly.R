context("Probability density")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("density-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(data=has.data, layout=L$layout)
}

# Draw a probability density estimation using geom_density
base <- ggplot(mtcars, aes(wt))

test_that("geom_density() is translated to area chart", {
  info <- expect_traces(base + geom_density(), 1, "simple")
  tr <- info$data[[1]]
  expect_identical(tr$type, "scatter")
  expect_identical(tr$fill, "tozeroy")
})

test_that("geom_density() respects fill aesthetic", {
  gg <- base + geom_density(aes(fill=factor(vs)), alpha = 0.3)
  info <- expect_traces(gg, 2, "fill")
  trs <- info$data
  type <- unique(sapply(trs, "[[", "type"))
  fill <- unique(sapply(trs, "[[", "fill"))
  expect_identical(type, "scatter")
  expect_identical(fill, "tozeroy")
})

test_that("geom_density() respects colour aesthetic", {
  info <- expect_traces(base + geom_density(aes(colour=factor(vs))), 2, "color")
  trs <- info$data
  type <- unique(sapply(trs, "[[", "type"))
  fill <- unique(sapply(trs, "[[", "fill"))
  expect_identical(type, "scatter")
  expect_identical(fill, "tozeroy")
})

g <- base + 
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "pink") +
  geom_density(fill = "lightblue", alpha = 0.1)
  
test_that("geom_histogram(aes(y = ..density..)) + geom_density() works", {
  info <- expect_traces(g, 2, "histogram")
  trs <- info$data
  type <- unique(sapply(trs, "[[", "type"))
  expect_identical(sort(type), c("bar", "scatter"))
})

# Check if the traces are in the correct order when position = stack
# Generate ggplot object
p <- ggplot(data = mtcars, aes(x = mpg, fill = factor(cyl))) + 
  geom_density(position = "stack")

test_that("traces are ordered correctly in geom_density", {
  info <- expect_traces(p, 3, "traces_order")
  nms <- sapply(info$data, "[[", "name")
  expect_identical(nms, c("8", "6", "4"))
})

