context("colorbar")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equivalent(length(L$data), n.traces)
  L
}


test_that("Can set colorbar attributes", {
  p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl)
  p <- colorbar(p, len = 0.5)
  l <- expect_traces(p, 2, "colorbar")
  expect_equivalent(l$data[[2]]$marker$colorbar$len, 0.5)
})


test_that("Can expand limits", {
  p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl)
  p <- colorbar(p, limits = c(0, 20))
  l <- expect_traces(p, 2, "colorbar-expand")
  expect_equivalent(l$data[[1]]$marker$cmin, 0)
  expect_equivalent(l$data[[2]]$marker$cmin, 0)
  expect_equivalent(l$data[[1]]$marker$cmax, 20)
  expect_equivalent(l$data[[2]]$marker$cmax, 20)
})

test_that("Can restrict limits", {
  p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl)
  p <- colorbar(p, limits = c(5, 7))
  l <- expect_traces(p, 2, "colorbar-restrict")
  expect_equivalent(unique(l$data[[1]]$marker$color), c(6, NA))
  expect_equivalent(l$data[[2]]$marker$cmin, 5)
  expect_equivalent(l$data[[2]]$marker$cmax, 7)
})

test_that("Can expand z limits", {
  p <- plot_ly(z = ~volcano)
  p <- colorbar(p, limits = c(0, 300))
  l <- expect_traces(p, 1, "colorbar-z-expand")
  expect_equivalent(l$data[[1]]$zmin, 0)
  expect_equivalent(l$data[[1]]$zmax, 300)
})

test_that("Can restrict z limits", {
  p <- plot_ly(z = ~volcano)
  p <- colorbar(p, limits = c(140, 160))
  l <- expect_traces(p, 1, "colorbar-z-restrict")
  expect_equivalent(l$data[[1]]$zmin, 140)
  expect_equivalent(l$data[[1]]$zmax, 160)
  v <- c(volcano)
  v[v < 140 | 160 < v] <- NA
  dim(v) <- dim(volcano)
  expect_equivalent(l$data[[1]][["z"]], v)
})
