context("plotly-color")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- expect_doppelganger_built(p, paste0("plotly-color-", name))
  expect_equivalent(length(L$data), n.traces)
  L
}

test_that("plot_ly() handles a simple scatterplot", {
  p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~bill_length_mm, color = ~bill_depth_mm)
})

test_that("Mapping a factor variable to color works", {
  p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~bill_length_mm, color = ~species)
  l <- expect_traces(p, 3, "scatterplot-color-factor")
  markers <- lapply(l$data, "[[", "marker")
  cols <- unlist(lapply(markers, "[[", "color"))
  expect_equivalent(length(cols), 3)
})

test_that("Custom RColorBrewer pallette works for factor variable", {
  cols <- RColorBrewer::brewer.pal(9, "Set1")
  # convert hex to rgba spec for comparison's sake
  colsToCompare <- toRGB(cols)
  # specifying a pallette set should "span the gamut" 
  p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~bill_length_mm, color = ~species, 
               colors = "Set1")
  l <- expect_traces(p, 3, "scatterplot-color-factor-custom")
  markers <- lapply(l$data, "[[", "marker")
  colz <- unlist(lapply(markers, "[[", "color"))
  idx <- if (packageVersion("scales") > '1.0.0') c(1, 2, 3) else c(1, 5, 9)
  expect_identical(sort(colsToCompare[idx]), sort(colz))
  # providing vector of RGB codes should also work
  p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~bill_length_mm, color = ~species, 
               colors = cols[1:3])
  l <- expect_traces(p, 3, "scatterplot-color-factor-custom2")
  markers <- lapply(l$data, "[[", "marker")
  colz <- unlist(lapply(markers, "[[", "color"))
  expect_identical(sort(colsToCompare[1:3]), sort(colz))
})

test_that("Passing hex codes to colors argument works", {
  colz <- c('#FE8268', '#81E8FE', '#FED681', '#81FED6', '#FE81A9')
  d <- data.frame(Category = LETTERS[1:5], Value = 1:5, stringsAsFactors = FALSE)
  p <- plot_ly(d, x = ~Category, y = ~Value, type = "bar", 
               color = ~Category, colors = colz)
  l <- expect_traces(p, 5, "bar-color-factor-custom")
  colz2 <- sapply(l$data, function(x) x[["marker"]][["color"]])
  expect_identical(sort(toRGB(colz)), sort(colz2))
})

test_that("Mapping a numeric variable to color works", {
  p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~bill_length_mm, color = ~bill_depth_mm)
  # one trace is for the colorbar
  l <- expect_traces(p, 2, "scatterplot-color-numeric")
  idx <- vapply(l$data, is.colorbar, logical(1))
  markerScale <- l$data[[which(idx)]]$marker
  markerDat <- l$data[[which(!idx)]]$marker
  expect_true(all(markerDat$color == na.omit(palmerpenguins::penguins$bill_depth_mm)))
  expect_true(markerScale$colorbar$title == "bill_depth_mm")
  expect_true(min(na.omit(palmerpenguins::penguins$bill_depth_mm)) == markerScale$cmin)
  expect_true(max(na.omit(palmerpenguins::penguins$bill_depth_mm)) == markerScale$cmax)
  expect_true(all(0 <= markerScale$colorscale[,1] & markerScale$colorscale[,1] <= 1))
})

test_that("color/stroke mapping with box translates correctly", {
  d <- data.frame(x = rep(c("A", "B"), each = 5), y = rnorm(10))
  l <- plot_ly(d) %>% 
    add_boxplot(x = ~x, y = ~y, color = ~x, colors = c('A' = "blue", 'B' = "red"), stroke = I("black")) %>%
    expect_traces(2, "box-color-stroke")
  expect_true(l$data[[1]]$fillcolor == toRGB("blue", 0.5))
  expect_true(l$data[[2]]$fillcolor == toRGB("red", 0.5))
  expect_true(l$data[[1]]$line$color == toRGB("black"))
  expect_true(l$data[[2]]$line$color == toRGB("black"))
})

test_that("Custom RColorBrewer pallette works for numeric variable", {
  p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~bill_length_mm, 
               color = ~bill_depth_mm, colors = "Greens")
  # one trace is for the colorbar
  l <- expect_traces(p, 2, "scatterplot-color-numeric-custom")
})

test_that("axis titles get attached to scene object for 3D plots", {
  p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~bill_depth_mm, z = ~flipper_length_mm)
  l <- expect_traces(p, 1, "scatterplot-scatter3d-axes")
  expect_identical(l$data[[1]]$type, "scatter3d")
  scene <- l$layout$scene
  expect_true(scene$xaxis$title == "bill_length_mm")
  expect_true(scene$yaxis$title == "bill_depth_mm")
  expect_true(scene$zaxis$title == "flipper_length_mm")
})

test_that("Can specify a scale manually", {
  pal <- c("1" = "red", "0" = "blue")
  p <- plot_ly(mtcars, x = ~mpg, y = ~disp, color = ~factor(vs), colors = pal)
  l <- expect_traces(p, 2, "color-manual")
  markers <- lapply(l$data, "[[", "marker")
  expected <- setNames(pal[sapply(l$data, "[[", "name")], NULL)
  expect_equivalent(toRGB(expected), sapply(markers, "[[", "color"))
})

test_that("attributes are boxed-up correctly", {
  df <- data.frame(
    a = rnorm(5, 50, 1), 
    b = letters[1:5], 
    stringsAsFactors = FALSE
  )
  
  p <- plot_ly(df, x = ~a, y = ~b, color = ~b) %>%
    add_bars(text = ~paste("Value: ", round(a, 1)), hoverinfo = "text")
  
  l <- plotly_build(p)$x
  
  expect_length(l$data, 5)
  
  for (i in seq_along(l$data)) {
    expect_is(l$data[[i]]$x, "AsIs")
    expect_is(l$data[[i]]$y, "AsIs")
    expect_length(l$data[[i]]$text, 1)
  }
  
})

