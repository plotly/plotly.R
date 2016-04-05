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

test_that("shareX produces one x-axis", {
  s <- subplot(plot_ly(x = 1), plot_ly(x = 1), nrows = 2, shareX = TRUE)
  l <- expect_traces(s, 2, "shareX")
  expect_true(sum(grepl("^xaxis", names(l$layout))) == 1)
})

test_that("shareY produces one y-axis", {
  s <- subplot(plot_ly(x = 1), plot_ly(x = 1), shareY = TRUE)
  l <- expect_traces(s, 2, "shareY")
  expect_true(sum(grepl("^yaxis", names(l$layout))) == 1)
})

test_that("share both axes", {
  s <- subplot(
    plot_ly(x = 1), plot_ly(x = 1), plot_ly(x = 1), plot_ly(x = 1), 
    nrows = 2, shareX = TRUE, shareY = TRUE
  )
  l <- expect_traces(s, 4, "shareBoth")
  expect_true(sum(grepl("^yaxis", names(l$layout))) == 2)
  expect_true(sum(grepl("^xaxis", names(l$layout))) == 2)
})

# https://github.com/ropensci/plotly/issues/376
library(plotly)
d <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)
hist_top <- ggplot(d) + geom_histogram(aes(x = x))
empty <- ggplot() + geom_blank()
scatter <- ggplot(d) + geom_point(aes(x = x, y = y))
hist_right <- ggplot(d) + geom_histogram(aes(x = y)) + coord_flip()
s <- subplot(
  hist_top, empty, scatter, hist_right, 
  nrows = 2, widths = c(0.8, 0.2), heights = c(0.2, 0.8),
  margin = 0.005, shareX = TRUE, shareY = TRUE
)

test_that("Row/column height/width", {
  l <- expect_traces(s, 3, "width-height")
  expect_equal(diff(l$layout$xaxis$domain), 0.8 - 0.005)
  expect_equal(diff(l$layout$xaxis2$domain), 0.2 - 0.005)
  expect_equal(diff(l$layout$yaxis$domain), 0.2 - 0.005)
  expect_equal(diff(l$layout$yaxis2$domain), 0.8 - 0.005)
})

