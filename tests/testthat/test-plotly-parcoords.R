expect_traces <- function(p, n.traces, name) {
  stopifnot(is.numeric(n.traces))
  L <- expect_doppelganger_built(p, paste0("plotly-", name))
  expect_equivalent(length(L$data), n.traces)
  L
}

test_that("values property has a class of AsIs", {
  p <- plot_ly(
    dimensions = list(
      list(label = "A", values = 3),
      list(label = "B", values = 8)
    ),
    type = "parcoords"
  )
  l <- expect_traces(p, 1, "parcoords-data-array")
  tr <- l$data[[1]]$dimensions
  expect_true(inherits(tr[[1]]$values, "AsIs"))
  expect_true(inherits(tr[[2]]$values, "AsIs"))
})
