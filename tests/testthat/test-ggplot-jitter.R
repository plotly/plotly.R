context("geom_jitter")

# Expect trace function
expect_traces <- function(gg, n_traces, name) {
  stopifnot(is.numeric(n_traces))
  save_outputs(gg, paste0("jitter-", name))
  L <- gg2list(gg)
  all_traces <- L$data
  no_data <- sapply(all_traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has_data <- all_traces[!no_data]
  expect_equivalent(length(has_data), n_traces)
  list(traces = has_data, layout = L$layout)
}

p <- ggplot(mpg, aes(cyl, hwy)) + geom_jitter()

test_that("geom_jitter is working", {
  info <- expect_traces(p, 1, "basic")
  tr <- info$traces[[1]]
  expect_identical(tr$type, "scatter")
  # default jitter is 40% of the resolution of the data.
  diffs <- abs(mpg$cyl - tr$x)
  expect_true(all(0 < diffs & diffs < 0.4))
})
