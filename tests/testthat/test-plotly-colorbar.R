context("colorbar")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equivalent(length(L$data), n.traces)
  L
}


test_that("Can set colorbar attributes", {
  p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl)
  p <- colorbar(p, len = 0.3)
  l <- expect_traces(p, 2, "colorbar")
  expect_equivalent(l$data[[2]]$marker$colorbar$len, 0.3)
})


test_that("Can expand limits", {
  p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl)
  p <- colorbar(p, limits = c(0, 20))
  l <- expect_traces(p, 2, "colorbar-expand")
  expect_true(l$data[[1]]$marker$cmin == 0)
  expect_true(l$data[[2]]$marker$cmin == 0)
  expect_true(l$data[[1]]$marker$cmax == 20)
  expect_true(l$data[[2]]$marker$cmax == 20)
})

test_that("Can restrict limits", {
  p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl)
  p <- colorbar(p, limits = c(5, 7))
  l <- expect_traces(p, 2, "colorbar-restrict")
  expect_equivalent(unique(l$data[[1]]$marker$color), c(6, NA))
  expect_true(l$data[[2]]$marker$cmin == 5)
  expect_true(l$data[[2]]$marker$cmax == 7)
})

test_that("Can expand z limits", {
  p <- plot_ly(z = ~volcano)
  p <- colorbar(p, limits = c(0, 300))
  l <- expect_traces(p, 1, "colorbar-z-expand")
  expect_equivalent(l$data[[1]]$zmin, 0)
  expect_equivalent(l$data[[1]]$zmax, 300)
})


# TODO: values outside the scale limits should probably be non-transparent (e.g. gray) 
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


test_that("colorbar does not affect mode of other traces", {
  # https://github.com/ropensci/plotly/issues/1196
  p <- plot_ly() %>% 
    add_markers(data = mtcars, x= ~ hp, y= ~mpg, color = ~wt) %>% 
    add_lines(x = seq(100, 300, length.out = 20), y = seq(10, 30, length.out = 20),
              color = I("black"))
  
  expect_true(hide_colorbar(p)$x$data[[1]]$mode == "markers")
  expect_true(hide_colorbar(p)$x$data[[2]]$mode == "lines")
  expect_true(colorbar(p, limits = c(1,10))$x$data[[1]]$mode == "markers")
  expect_true(colorbar(p, limits = c(1,10))$x$data[[2]]$mode == "lines")
  
})


test_that("can control both fill and stroke colorbars", {
  
  p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl, stroke = ~wt) %>%
    colorbar(title = "fill color", len = 0.4) %>%
    colorbar(title = "stroke color", len = 0.6, y = 0.55, which = 2)
  
  d <- p$x$data
  expect_length(d, 3)
  
  bar_fill <- d[[2]]$marker$colorbar
  expect_true(bar_fill$len == 0.4)
  expect_true(bar_fill$y == 1)
  expect_true(bar_fill$title == "fill color")
  
  bar_stroke <- d[[3]]$marker$colorbar
  expect_true(bar_stroke$len == 0.6)
  expect_true(bar_stroke$y == 0.55)
  expect_true(bar_stroke$title == "stroke color")
  
})
