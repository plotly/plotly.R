context("plot_ly")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equal(length(L$data), n.traces)
  L
}

test_that("plot_ly() handles a simple scatterplot", {
  p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, mode = "markers")
  l <- expect_traces(p, 1, "scatterplot")
  expect_identical(l$data[[1]]$mode, "markers")
  expect_identical(l$data[[1]]$x, iris$Sepal.Length)
  expect_identical(l$data[[1]]$y, iris$Petal.Length)
  expect_identical(l$layout$xaxis$title, "Sepal.Length")
  expect_identical(l$layout$yaxis$title, "Petal.Length")
})


test_that("Mapping a variable to symbol works", {
  p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, symbol = ~Species)
  expect_message(plotly_build(p), "Adding markers to mode")
  l <- expect_traces(p, 3, "scatterplot-symbol")
  markers <- lapply(l$data, "[[", "marker")
  syms <- unlist(lapply(markers, "[[", "symbol"))
  expect_identical(syms, c("circle", "cross", "diamond"))
})

test_that("Mapping a factor variable to color works", {
  p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, 
               color = ~Species, mode = "markers")
  l <- expect_traces(p, 3, "scatterplot-color-factor")
  markers <- lapply(l$data, "[[", "marker")
  cols <- unlist(lapply(markers, "[[", "color"))
  expect_equal(length(cols), 3)
})

test_that("Custom RColorBrewer pallette works for factor variable", {
  cols <- RColorBrewer::brewer.pal(9, "Set1")
  # specifying a pallette set should "span the gamut" 
  p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, 
               color = ~Species, colors = "Set1", mode = "markers")
  l <- expect_traces(p, 3, "scatterplot-color-factor-custom")
  markers <- lapply(l$data, "[[", "marker")
  colz <- unlist(lapply(markers, "[[", "color"))
  expect_identical(sort(cols[c(1, 5, 9)]), sort(colz))
  # providing vector of RGB codes should also work
  p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, 
               color = ~Species, colors = cols[1:3], mode = "markers")
  l <- expect_traces(p, 3, "scatterplot-color-factor-custom2")
  markers <- lapply(l$data, "[[", "marker")
  colz <- unlist(lapply(markers, "[[", "color"))
  expect_identical(sort(cols[1:3]), sort(colz))
})

test_that("Passing hex codes to colors argument works", {
  colz <- c('#FE8268', '#81E8FE', '#FED681', '#81FED6', '#FE81A9')
  d <- data.frame(Category = LETTERS[1:5], Value = 1:5, stringsAsFactors = F)
  p <- plot_ly(d, x = ~Category, y = ~Value, type = "bar", 
               color = ~Category, colors = colz)
  l <- expect_traces(p, 5, "bar-color-factor-custom")
  colz2 <- sapply(l$data, function(x) x[["marker"]][["color"]])
  expect_identical(sort(colz), sort(colz2))
})

test_that("Mapping a numeric variable to color works", {
  p <- plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, 
               color = ~Petal.Width, mode = "markers")
  # one trace is for the colorbar
  l <- expect_traces(p, 2, "scatterplot-color-numeric")
  idx <- vapply(l$data, is.colorbar, logical(1))
  markerScale <- l$data[[which(idx)]]$marker
  markerDat <- l$data[[which(!idx)]]$marker
  expect_identical(markerDat$color, iris$Petal.Width)
  expect_identical(markerScale$colorbar$title, "Petal.Width")
  expect_equal(min(iris$Petal.Width), markerScale$cmin)
  expect_equal(max(iris$Petal.Width), markerScale$cmax)
  expect_true(all(0 <= markerScale$colorscale[,1] & markerScale$colorscale[,1] <= 1))
})

test_that("Custom RColorBrewer pallette works for numeric variable", {
  p <- plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, 
               color = ~Petal.Width, colors = "Greens", mode = "markers")
  # one trace is for the colorbar
  l <- expect_traces(p, 2, "scatterplot-color-numeric-custom")
})

test_that("axis titles get attached to scene object for 3D plots", {
  p <- plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width, z = ~Sepal.Width,
               type = "scatter3d", mode = "markers")
  l <- expect_traces(p, 1, "scatterplot-scatter3d-axes")
  scene <- l$layout$scene
  expect_identical(scene$xaxis$title, "Petal.Length")
  expect_identical(scene$yaxis$title, "Petal.Width")
  expect_identical(scene$zaxis$title, "Sepal.Width")
})

test_that("type inference + add_data + layering works as expected", {
p <- plot_ly(iris, x = ~Species) %>% 
  add_trace(opacity = 0.3) %>%
  add_data(iris[sample(nrow(iris), 10), ]) %>% 
  add_trace() %>%
  layout(barmode = "overlay")
  l <- expect_traces(p, 2, "bar-inference")
  types <- unique(unlist(lapply(l$data, "[[", "type")))
  expect_equal(types, "histogram")
  expect_equal(l$data[[1]]$opacity, 0.3)
  expect_equal(l$layout$barmode, "overlay")
  expect_true(length(l$data[[1]]$x) > length(l$data[[2]]$x))
})

test_that("x/y/z properties have a class of AsIs", {
  p <- plot_ly(x = 1, y = 1, z = 1, type = "scatter3d")
  l <- expect_traces(p, 1, "box-data-array")
  tr <- l$data[[1]]
  expect_true(inherits(tr$x, "AsIs"))
  expect_true(inherits(tr$y, "AsIs"))
  expect_true(inherits(tr$z, "AsIs"))
})

test_that("grouping within multiples traces works", {
  g <- expand.grid(visit = 1:2, id = 1:3, cohort = c("A", "B"))
  g$response <- rnorm(nrow(g))
  d <- group_by(g, id)
  p <- plot_ly(d, x = ~visit, y = ~response, color = ~cohort, colors = c("red", "blue"))
  l <- expect_traces(p, 2, "group-within-trace")
  expect_equal(l$data[[1]]$x, c(1, 2, NA, 1, 2, NA, 1, 2))
  expect_equal(l$data[[2]]$x, c(1, 2, NA, 1, 2, NA, 1, 2))
  expect_true(l$data[[1]]$marker$color == "#FF0000")
  expect_true(l$data[[1]]$line$color == "#FF0000")
  expect_true(l$data[[2]]$marker$color == "#0000FF")
  expect_true(l$data[[2]]$line$color == "#0000FF")
})
