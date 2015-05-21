context("geom_jitter")

# Expect trace function
expect_traces <- function(gg, n_traces, name, seed) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n_traces))
  save_outputs(gg, paste0("coord_fixed-", name))
  set.seed(seed)
  L <- gg2list(gg)
  all_traces <- L$data
  no_data <- sapply(all_traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has_data <- all_traces[!no_data]
  expect_equal(length(has_data), n_traces)
  list(traces = has_data, layout = L$layout)
}

#head(L$data[[1]]$x)

# get data from mpg dataset
p <- ggplot(mpg, aes(displ, hwy))

# Test 1
# set up the data
set.seed(1001)
p1 <- ggplot() + geom_jitter(data = mpg, aes(displ, hwy), width = 1)
head(ggplot_build2(p1)$data[[1]]$x)
# test
test_that("geom_jitter is working", {
  info <- expect_traces(p1, 1, "geom_jitter", 1001)
  tr <- info$traces[[1]]
  la <- info$layout
  expect_identical(tr$type, "scatter")
  set.seed(1001)
  built <- ggplot_build2(p1)
  print(head(tr$x)) # from gg2list
  print(head(built$data[[1]]$x)) # from ggplot_build2
  expect_identical(tr$x, built$data[[1]]$x)
  expect_identical(tr$y, built$data[[1]]$y)
})

