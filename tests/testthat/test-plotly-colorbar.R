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

test_that("positioning with multiple colorbars and legends", {
  
  s <- subplot(
    plot_ly(z = ~volcano),
    plot_ly(x = 1:10, y = 1:10, color = 1:10)
  )
  
  b <- plotly_build(s)
  d <- b$x$data
  expect_length(d, 3)
  
  expect_true(d[[1]]$colorbar$len == 1/3)
  expect_true(d[[1]]$colorbar$lenmode == "fraction")
  expect_true(d[[1]]$colorbar$yanchor == "top")
  expect_true(d[[1]]$colorbar$y == 1)
  
  expect_true(d[[3]]$marker$colorbar$len == 1/3)
  expect_true(d[[3]]$marker$colorbar$lenmode == "fraction")
  expect_true(d[[3]]$marker$colorbar$yanchor == "top")
  expect_equal(as.numeric(d[[3]]$marker$colorbar$y), 2/3, tolerance = 0.01)
  
  expect_true(b$x$layout$legend$yanchor == "top")
  expect_equal(as.numeric(b$x$layout$legend$y), 1/3, tolerance = 0.01)
  
  
  s <- subplot(
    plot_ly(z = ~volcano),
    plot_ly(x = 1:10, y = 1:10, color = factor(1:10))
  )
  
  b <- plotly_build(s)
  d <- b$x$data
  expect_length(d, 11)
  
  expect_true(d[[1]]$colorbar$len == 0.5)
  expect_true(d[[1]]$colorbar$lenmode == "fraction")
  expect_true(d[[1]]$colorbar$yanchor == "top")
  expect_true(d[[1]]$colorbar$y == 1)
  
  expect_true(b$x$layout$legend$y == 0.5)
  expect_true(b$x$layout$legend$yanchor == "top")
})


test_that("Colorbar limits controls marker.color and line.color", {
  
  # https://github.com/ropensci/plotly/issues/1236
  p <- plot_ly(
    mtcars, x = ~hp, y = ~cyl, z = ~mpg, color = ~mpg,
    type = "scatter3d", mode = "lines+markers"
  )
  
  b <- plotly_build(p)
  expect_length(b$x$data, 2)
  expect_true(all(b$x$data[[1]]$marker$color == mtcars$mpg))
  expect_true(all(b$x$data[[1]]$line$color == mtcars$mpg))
  expect_true(b$x$data[[1]]$marker$cmin == min(mtcars$mpg))
  expect_true(b$x$data[[1]]$marker$cmax == max(mtcars$mpg))
  expect_true(b$x$data[[1]]$line$cmin == min(mtcars$mpg))
  expect_true(b$x$data[[1]]$line$cmax == max(mtcars$mpg))
  
  p2 <- colorbar(p, limits = c(0, 100))
  b2 <- plotly_build(p2)
  expect_length(b2$x$data, 2)
  expect_true(all(b2$x$data[[1]]$marker$color == mtcars$mpg))
  expect_true(all(b2$x$data[[1]]$line$color == mtcars$mpg))
  expect_true(b2$x$data[[1]]$marker$cmin == 0)
  expect_true(b2$x$data[[1]]$marker$cmax == 100)
  expect_true(b2$x$data[[1]]$line$cmin == 0)
  expect_true(b2$x$data[[1]]$line$cmax == 100)
  
  p3 <- colorbar(p, limits = c(20, 100))
  b3 <- plotly_build(p3)
  mpg <- mtcars$mpg
  mpg[mpg < 20] <- NA
  expect_true(Reduce(`&`, Map(identical, b3$x$data[[1]]$marker$color, mpg)))
  expect_true(Reduce(`&`, Map(identical, b3$x$data[[1]]$line$color, mpg)))
  expect_true(b3$x$data[[1]]$marker$cmin == 20)
  expect_true(b3$x$data[[1]]$marker$cmax == 100)
  expect_true(b3$x$data[[1]]$line$cmin == 20)
  expect_true(b3$x$data[[1]]$line$cmax == 100)
})

test_that("colorbar limits shouldn't control non-color-scale mapping(s)", {
  
  p <- plot_ly(x = 1:10, y = 1:10, color = 1:10) %>% 
    add_markers() %>% 
    add_lines(x  = 1:3, y = 1:3, color = I('red')) %>% 
    colorbar(limits = c(1, 5))
  
  b <- plotly_build(p)
  expect_length(b$x$data, 3)
  
  expect_true(sum(is.na(b$x$data[[1]]$marker$color)) == 5)
  expect_true(b$x$data[[2]]$line$color == toRGB("red"))
})
