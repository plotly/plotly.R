context("plotly-symbol")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equal(length(L$data), n.traces)
  L
}

test_that("Mapping a variable to symbol works", {
  p <- plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, symbol = ~Species)
  l <- expect_traces(p, 3, "scatterplot-symbol")
  markers <- lapply(l$data, "[[", "marker")
  syms <- unlist(lapply(markers, "[[", "symbol"))
  expect_identical(syms, c("circle", "cross", "diamond"))
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
