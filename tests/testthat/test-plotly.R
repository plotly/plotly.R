context("plot_ly")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equal(length(L$data), n.traces)
  L
}

test_that("plot_ly() handles a simple scatterplot", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, mode = "markers")
  l <- expect_traces(p, 1, "basic-scatterplot")
  expect_identical(l$data[[1]]$mode, "markers")
  expect_identical(l$data[[1]]$x, iris$Sepal.Length)
  expect_identical(l$data[[1]]$y, iris$Petal.Length)
  expect_identical(l$layout$xaxis$title, "Sepal.Length")
  expect_identical(l$layout$yaxis$title, "Petal.Length")
})

test_that("plot_ly() creates a separate trace for each (factor) level of color", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, 
               color = Species, mode = "markers")
  l <- expect_traces(p, 3, "basic-scatterplot-color-factor")
  markers <- lapply(l$data, "[[", "marker")
  cols <- unlist(lapply(markers, "[[", "color"))
  # TODO: test for the default color scheme?
  expect_equal(length(cols), 3)
})

test_that("plot_ly() creates a separate trace for each (factor) level of color", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, 
               color = Petal.Width, mode = "markers")
  l <- expect_traces(p, 1, "basic-scatterplot-color-numeric")
  marker <- l$data[[1]]$marker
  expect_identical(marker$colorbar$title, "Petal.Width")
  expect_identical(marker$color, iris$Petal.Width)
  expect_identical(marker$cmin, min(iris$Petal.Width))
  expect_identical(marker$cmax, max(iris$Petal.Width))
  expect_true(all(0 <= marker$colorscale[,1] & marker$colorscale[,1] <= 1))
})
