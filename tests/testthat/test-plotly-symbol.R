context("plotly-symbol")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-symbol-", name))
  expect_equivalent(length(L$data), n.traces)
  L
}

test_that("Mapping a variable to symbol works", {
  p <- plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, symbol = ~Species)
  l <- expect_traces(p, 3, "scatterplot-symbol")
  markers <- lapply(l$data, "[[", "marker")
  syms <- unlist(lapply(markers, "[[", "symbol"))
  expect_identical(syms, c("circle", "triangle-up", "square"))
})

test_that("Can set the symbol range.", {
  p <- plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, symbol = ~Species, 
               symbols = 1:3)
  l <- expect_traces(p, 3, "scatterplot-symbol2")
  markers <- lapply(l$data, "[[", "marker")
  syms <- unlist(lapply(markers, "[[", "symbol"))
  expect_identical(syms, plotly:::pch2symbol(1:3))
})


test_that("Setting a constant symbol works", {
  p <- plot_ly(iris, x = 1:25, y = 1:25, symbol = I(0:24))
  l <- expect_traces(p, 1, "pch")
  markers <- lapply(l$data, "[[", "marker")
  syms <- unlist(lapply(markers, "[[", "symbol"))
  expect_identical(syms, plotly:::pch2symbol(0:24))
})

test_that("Warn about invalid symbol codes", {
  p <- plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, symbol = I("DNE"))
  expect_warning(plotly_build(p), "DNE")
})

test_that("Formula resulting in logical vector works", {
  s <- c("triangle-up", "circle-open")
  p <- plot_ly(x = 1:10, y = 1:10, symbol = ~1:10 > 5, symbols = s)
  l <- expect_traces(p, 2, "logical")
  markers <- lapply(l$data, "[[", "marker")
  syms <- unlist(lapply(markers, "[[", "symbol"))
  expect_identical(syms, s)
})

test_that("Can specify a scale manually", {
  pal <- c("1" = "cross", "0" = "diamond")
  p <- plot_ly(mtcars, x = ~mpg, y = ~disp, symbol = ~factor(vs), symbols = pal)
  l <- expect_traces(p, 2, "symbol-manual")
  markers <- lapply(l$data, "[[", "marker")
  expected <- setNames(pal[sapply(l$data, "[[", "name")], NULL)
  expect_equivalent(expected, sapply(markers, "[[", "symbol"))
})

test_that("Trace ordering matches factor levels", {
  p <- plot_ly(mtcars, x = ~mpg, y = ~disp, symbol = ~factor(vs, levels = c(1, 0)))
  l <- expect_traces(p, 2, "ordering")
  expect_equivalent(sapply(l$data, "[[", "name"), c("1", "0"))
})

test_that("Trace ordering is alphabetical", {
  lvls <- sort(unique(mpg$class))
  p <- plot_ly(mpg, x = ~cty, y = ~hwy, symbol = ~class)
  l <- expect_traces(p, length(lvls), "alphabetical")
  expect_equivalent(sapply(l$data, "[[", "name"), lvls)
})
