context("geom_rect")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("rects-", name))
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

set.seed(1)
df <- data.frame(
  x = sample(10, 20, replace = TRUE),
  y = sample(10, 20, replace = TRUE)
)

gg <- ggplot(df, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 2)) +
  geom_rect()

test_that('geom_rect becomes traces with mode "lines" with fill "tozerox"', {
  info <- expect_traces(gg, 1, "black")
  tr <- info$traces[[1]]
  expect_identical(tr$fill, "tozerox")
  expect_identical(tr$type, "scatter")
  expect_identical(tr$mode, "lines")
  for(xy in c("x", "y")){
    expect_true(any(is.na(tr[[xy]])))
  }
})
