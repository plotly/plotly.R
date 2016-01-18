context("subplot")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-subplot-", name))
  expect_equal(length(L$data), n.traces)
  L
}

test_that("simple subplot works", {
  p1 <- plot_ly(x = c(1, 2))
  p2 <- plot_ly(x = c(1, 2))
  s <- expect_traces(subplot(p1, p2), 2, "simple")
  expect_identical(s$data[[2]]$xaxis, s$layout[["yaxis2"]][["anchor"]])
  expect_identical(s$data[[2]]$yaxis, s$layout[["xaxis2"]][["anchor"]])
  doms <- lapply(s$layout, "[[", "domain")
  expect_true(doms$xaxis[2] <= doms$xaxis2[1])
})

test_that("nrows argument works", {
  p1 <- plot_ly(x = c(1, 2))
  p2 <- plot_ly(x = c(1, 2))
  s <- expect_traces(subplot(p1, p2, nrows = 2), 2, "simple2")
  expect_identical(s$data[[2]]$xaxis, s$layout[["yaxis2"]][["anchor"]])
  expect_identical(s$data[[2]]$yaxis, s$layout[["xaxis2"]][["anchor"]])
  doms <- lapply(s$layout, "[[", "domain")
  expect_true(doms$yaxis[2] > doms$yaxis[1])
  expect_true(doms$yaxis[1] > doms$yaxis2[2])
  expect_true(doms$yaxis2[2] > doms$yaxis2[1])
})

test_that("group + [x/y]axis works", {
  iris$id <- as.integer(iris$Species)
  p <- plot_ly(iris, x = Petal.Length, y = Petal.Width, group = Species,
               xaxis = paste0("x", id), mode = "markers")
  s <- expect_traces(subplot(p, margin = 0.05), 3, "group")
  ax <- s$layout[grepl("^[x-y]axis", names(s$layout))]
  doms <- lapply(ax, "[[", "domain")
  # make sure y domain is [0, 1] on every axis
  ydom <- doms[grepl("^y", names(doms))]
  expect_equal(sort(unique(unlist(ydom))), c(0, 1))
  xdom <- doms[grepl("^x", names(doms))]
  expect_true(all(1/3 > xdom[[1]] & xdom[[1]] >= 0))
  expect_true(all(2/3 > xdom[[2]] & xdom[[2]] > 1/3))
  expect_true(all(1 >= xdom[[3]] & xdom[[3]] > 2/3))
})



