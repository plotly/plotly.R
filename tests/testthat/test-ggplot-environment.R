context("Objects and Environments")

# Expect trace function
expect_traces <- function(gg, n_traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n_traces))
  save_outputs(gg, paste0("object_environments-", name))
  L <- gg2list(gg)
  all_traces <- L$data
  no_data <- sapply(all_traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has_data <- all_traces[!no_data]
  expect_equal(length(has_data), n_traces)
  list(traces = has_data, layout = L$layout)
}

# make data
set.seed(955)
dat <- data.frame(cond = rep(c("A", "B"), each=10),
                  xvar = 1:20 + rnorm(20,sd=3),
                  yvar = 1:20 + rnorm(20,sd=3))

# make ggplot
p <- ggplot(dat, aes(x = xvar, y = yvar, color = cond)) + 
  geom_point() + xlab("X") + ylab("Y")

# Test 1: annotation
test_that("object annotations in environment outside plotly", {
  annotations <- "outside of the plotly environment"
  info <- expect_traces(p, 2, "annotations")
  tr <- info$traces[[1]]
  la <- info$layout
  expect_identical(tr$type, "scatter")
  expect_identical(la$xaxis$title, "X")
  expect_identical(la$yaxis$title, "Y")
  expect_true(grepl("cond", la$annotations[[1]]$text))
})

# Test 2: increase_margin_r
test_that("object increase_margin_r in environment outside plotly", {
  increase_margin_r <- "outside of the plotly environment"
  info <- expect_traces(p, 2, "increase_margin_r")
  tr <- info$traces[[1]]
  la <- info$layout
  expect_identical(la$xaxis$title, "X")
  expect_identical(la$yaxis$title, "Y")
  expect_identical(tr$type, "scatter")
  expect_true(grepl("cond", la$annotations[[1]]$text))
})

# Test 3: bargap
test_that("object bargap in environment outside plotly", {
  increase_margin_r <- "outside of the plotly environment"
  info <- expect_traces(p, 2, "bargap")
  tr <- info$traces[[1]]
  la <- info$layout
  expect_identical(la$xaxis$title, "X")
  expect_identical(la$yaxis$title, "Y")
  expect_identical(tr$type, "scatter")
  expect_true(grepl("cond", la$annotations[[1]]$text))
})

