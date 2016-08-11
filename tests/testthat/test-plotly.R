context("plotly")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equal(length(L$data), n.traces)
  L
}

# expect 2 plotly graphs to have the same traces
expect_same_data <- function(p1, p2) {
  if (!is.plotly(p1) || !is.plotly(p2)) {
    stop("Both arguments must be plotly objects", call. = FALSE)
  }
  d1 <- plotly_build(p1)$x$data
  d2 <- plotly_build(p2)$x$data
  if (length(d1) != length(d2)) {
    stop("Number of traces is different.", call. = FALSE)
  }
  # for each trace, align the names (since ordering doesn't matter)
  d1 <- Map(function(x, y) structure(x[names(y)], class = oldClass(x)), d1, d2)
  expect_identical(d1, d2)
}

test_that("vector values with repeated values are returned verbatim", {
  p <- plot_ly(x = c(1, 2), y = c(1, 1))
  l <- plotly_build(p)$x
  expect_identical(l$data[[1]]$x, c(1, 2))
  expect_identical(l$data[[1]]$y, c(1, 1))
})

test_that("plot_ly defaults to scatterplot", {
  p1 <- plot_ly(mtcars, x = ~wt, y = ~mpg)
  p2 <- plot_ly(mtcars, x = ~wt, y = ~mpg) %>% add_markers()
  expect_same_data(p1, p2)
})

test_that("Variable mappings return same result regardless of where they appear", {
  p1 <- plot_ly(mtcars, x = ~wt, y = ~mpg, size = ~disp)
  p2 <- plot_ly(mtcars, x = ~wt, y = ~mpg) %>% add_markers(size = ~disp)
  expect_same_data(p1, p2)
  p1 <- plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~disp)
  p2 <- plot_ly(mtcars, x = ~wt, y = ~mpg) %>% add_markers(color = ~disp)
  expect_same_data(p1, p2)
  p1 <- plot_ly(mtcars, x = ~wt, y = ~mpg, symbol = ~factor(am))
  p2 <- plot_ly(mtcars, x = ~wt, y = ~mpg) %>% add_markers(symbol = ~factor(am))
  expect_same_data(p1, p2)
  p1 <- plot_ly(mtcars, x = ~wt, y = ~mpg, linetype = ~factor(am))
  p2 <- plot_ly(mtcars, x = ~wt, y = ~mpg) %>% add_markers(linetype = ~factor(am))
  expect_message(plotly_build(p1), "Adding lines to mode")
  expect_message(plotly_build(p2), "Adding lines to mode")
  expect_same_data(p1, p2)
})



test_that("plot_ly() handles a simple scatterplot", {
  p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, mode = "markers")
  l <- expect_traces(p, 1, "scatterplot")
  expect_identical(l$data[[1]]$mode, "markers")
  expect_identical(l$data[[1]]$x, iris$Sepal.Length)
  expect_identical(l$data[[1]]$y, iris$Petal.Length)
  expect_identical(l$layout$xaxis$title, "Sepal.Length")
  expect_identical(l$layout$yaxis$title, "Petal.Length")
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
  l <- expect_traces(add_lines(p), 2, "group-within-trace")
  expect_equal(l$data[[1]]$x, c(1, 2, NA, 1, 2, NA, 1, 2))
  expect_equal(l$data[[2]]$x, c(1, 2, NA, 1, 2, NA, 1, 2))
  expect_true(l$data[[1]]$line$color == "#FF0000")
  expect_true(l$data[[2]]$line$color == "#0000FF")
})

test_that("Alpha can be applied to both constant and scaled colors", {
  p <- plot_ly(x = rnorm(100), y = rnorm(100), color = ~rnorm(100)) 
  p <- add_markers(p, alpha = 0.05)
  p <- add_lines(p, x = -1:1, y = -1:1, color = I("red"), alpha = 0.4)
  # one trace for the colorbar
  l <- expect_traces(p, 3, "alpha-blending")
  # verify the correct alpha for the points
  rgbs <- l$data[[1]]$marker$colorscale[, 2]
  alphas <- unique(sub("\\)", "", sapply(strsplit(rgbs, ","), "[[", 4)))
  expect_equal("0.05", alphas)
  # verify the correct alpha for the lines
  rgb <- l$data[[2]]$line$color
  alpha <- sub("\\)", "", sapply(strsplit(rgb, ","), "[[", 4))
  expect_equal("0.4", alpha)
})

test_that("Factors correctly mapped to a positional axis", {
  x <- factor(c(1, 2, 4, 8, 16, 32))
  p <- plot_ly(x = x, y = c(1, 2, 3, 4, 5, 6)) %>% add_markers()
  l <- expect_traces(p, 1, "factor-axis")
  expect_equal(l$layout$xaxis$type, "category")
  expect_equal(l$layout$xaxis$categoryorder, "array")
  expect_equal(l$layout$xaxis$categoryarray, levels(x))
})

test_that("Character strings correctly mapped to a positional axis", {
  # scramble alphabet order
  letters <- LETTERS[as.numeric(sort(as.character(1:26)))]
  p <- plot_ly(x = letters, y = seq_along(letters)) %>% 
    add_bars(color = rep(c("a1", "a2"), length.out = 26))
  l <- expect_traces(p, 2, "character-axis")
  expect_equal(l$layout$xaxis$type, "category")
  expect_equal(l$layout$xaxis$categoryorder, "array")
  expect_equal(l$layout$xaxis$categoryarray, LETTERS)
})
