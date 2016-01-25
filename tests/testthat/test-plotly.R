context("plot_ly")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equal(length(L$data), n.traces)
  L
}

test_that("plot_ly() handles a simple scatterplot", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, mode = "markers")
  l <- expect_traces(p, 1, "scatterplot")
  expect_identical(l$data[[1]]$mode, "markers")
  expect_identical(l$data[[1]]$x, iris$Sepal.Length)
  expect_identical(l$data[[1]]$y, iris$Petal.Length)
  expect_identical(l$layout$xaxis$title, "Sepal.Length")
  expect_identical(l$layout$yaxis$title, "Petal.Length")
})

test_that("Using group argument creates multiple traces", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, group = Species)
  l <- expect_traces(p, 3, "scatterplot-group")
  expect_identical(l$layout$xaxis$title, "Sepal.Length")
  expect_identical(l$layout$yaxis$title, "Petal.Length")
})

test_that("Mapping a variable to symbol works", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, symbol = Species)
  l <- expect_traces(p, 3, "scatterplot-symbol")
  markers <- lapply(l$data, "[[", "marker")
  syms <- unlist(lapply(markers, "[[", "symbol"))
  expect_identical(syms, c("dot", "cross", "diamond"))
})

test_that("Mapping a factor variable to color works", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, 
               color = Species, mode = "markers")
  l <- expect_traces(p, 3, "scatterplot-color-factor")
  markers <- lapply(l$data, "[[", "marker")
  cols <- unlist(lapply(markers, "[[", "color"))
  expect_equal(length(cols), 3)
})

test_that("Custom RColorBrewer pallette works for factor variable", {
  cols <- RColorBrewer::brewer.pal(9, "Set1")
  # specifying a pallette set should "span the gamut" 
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, 
               color = Species, colors = "Set1", mode = "markers")
  l <- expect_traces(p, 3, "scatterplot-color-factor-custom")
  markers <- lapply(l$data, "[[", "marker")
  colz <- unlist(lapply(markers, "[[", "color"))
  expect_identical(sort(cols[c(1, 5, 9)]), sort(colz))
  # providing vector of RGB codes should also work
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, 
               color = Species, colors = cols[1:3], mode = "markers")
  l <- expect_traces(p, 3, "scatterplot-color-factor-custom2")
  markers <- lapply(l$data, "[[", "marker")
  colz <- unlist(lapply(markers, "[[", "color"))
  expect_identical(sort(cols[1:3]), sort(colz))
})

test_that("Passing hex codes to colors argument works", {
  colz <- c('#FE8268', '#81E8FE', '#FED681', '#81FED6', '#FE81A9')
  d <- data.frame(Category = LETTERS[1:5], Value = 1:5, stringsAsFactors = F)
  p <- plot_ly(d, x = Category, y = Value, type = "bar", 
               color = Category, colors = colz)
  l <- expect_traces(p, 5, "bar-color-factor-custom")
  colz2 <- sapply(l$data, function(x) x[["marker"]][["color"]])
  expect_identical(sort(colz), sort(colz2))
})

test_that("Mapping a numeric variable to color works", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, 
               color = Petal.Width, mode = "markers")
  l <- expect_traces(p, 1, "scatterplot-color-numeric")
  marker <- l$data[[1]]$marker
  expect_identical(marker$colorbar$title, "Petal.Width")
  expect_identical(marker$color, iris$Petal.Width)
  expect_true(all(0 <= marker$colorscale[,1] & marker$colorscale[,1] <= 1))
})

test_that("Custom RColorBrewer pallette works for numeric variable", {
  p <- plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, 
               color = Petal.Width, colors = "Greens", mode = "markers")
  l <- expect_traces(p, 1, "scatterplot-color-numeric-custom")
  marker <- l$data[[1]]$marker
  expect_identical(marker$colorbar$title, "Petal.Width")
  expect_identical(marker$color, iris$Petal.Width)
  expect_true(all(0 <= marker$colorscale[,1] & marker$colorscale[,1] <= 1))
})

test_that("axis titles get attached to scene object for 3D plots", {
  p <- plot_ly(iris, x = Petal.Length, y = Petal.Width, z = Sepal.Width,
               type = "scatter3d", mode = "markers")
  l <- expect_traces(p, 1, "scatterplot-scatter3d-axes")
  scene <- l$layout$scene
  expect_identical(scene$xaxis$title, "Petal.Length")
  expect_identical(scene$yaxis$title, "Petal.Width")
  expect_identical(scene$zaxis$title, "Sepal.Width")
})

test_that("inheriting properties works as expected", {
  library(dplyr)
  p <- iris %>%
    count(Species) %>%
    plot_ly(x = Species, y = n, opacity = 0.5, type = "bar", inherit = TRUE) %>%
    layout(barmode = "overlay", showlegend = FALSE)
  s <- count(iris[sample(nrow(iris), 10), ], Species)
  p2 <- add_trace(p, data = s)
  l <- plotly_build(p2)
  expect_equal(l$data[[2]]$opacity, 0.5)
  expect_true(all(l$data[[1]]$y > l$data[[2]]$y))
})

test_that("x/y/z properties have a class of AsIs", {
  p <- plot_ly(x = 1, y = 1, z = 1, type = "scatter3d")
  l <- plotly_build(p)
  tr <- l$data[[1]]
  expect_true(inherits(tr$x, "AsIs"))
  expect_true(inherits(tr$y, "AsIs"))
  expect_true(inherits(tr$z, "AsIs"))
})
