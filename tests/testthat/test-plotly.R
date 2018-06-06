context("plotly")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equivalent(length(L$data), n.traces)
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
  expect_equivalent(l$data[[1]]$x, c(1, 2))
  expect_equivalent(l$data[[1]]$y, c(1, 1))
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
  expect_equivalent(l$data[[1]]$mode, "markers")
  expect_equivalent(l$data[[1]]$x, iris$Sepal.Length)
  expect_equivalent(l$data[[1]]$y, iris$Petal.Length)
  expect_true(l$layout$xaxis$title == "Sepal.Length")
  expect_true(l$layout$yaxis$title == "Petal.Length")
  expect_true(l$layout$xaxis$automargin)
  expect_true(l$layout$yaxis$automargin)
})

test_that("type inference + add_data + layering works as expected", {
p <- plot_ly(iris, x = ~Species) %>% 
  add_trace(opacity = 0.3) %>%
  add_data(iris[sample(nrow(iris), 10), ]) %>% 
  add_trace() %>%
  layout(barmode = "overlay")
  l <- expect_traces(p, 2, "bar-inference")
  types <- unique(unlist(lapply(l$data, "[[", "type")))
  expect_equivalent(types, "histogram")
  expect_equivalent(l$data[[1]]$opacity, 0.3)
  expect_equivalent(l$layout$barmode, "overlay")
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
  expect_equivalent(l$data[[1]]$x, c(1, 2, NA, 1, 2, NA, 1, 2))
  expect_equivalent(l$data[[2]]$x, c(1, 2, NA, 1, 2, NA, 1, 2))
  expect_true(l$data[[1]]$line$color == toRGB("red"))
  expect_true(l$data[[2]]$line$color == toRGB("blue"))
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
  expect_equivalent("0.05", alphas)
  # verify the correct alpha for the lines
  rgb <- l$data[[2]]$line$color
  alpha <- sub("\\)", "", sapply(strsplit(rgb, ","), "[[", 4))
  expect_equivalent("0.4", alpha)
})


test_that("Alpha still applies when no color is applied", {
  p <- plot_ly(mtcars, x = ~mpg, y = ~disp, alpha = 0.5) 
  l <- expect_traces(p, 1, "alpha-no-color")
  # verify the correct alpha for the points
  expect_true(l$data[[1]]$marker$color == "rgba(31,119,180,0.5)")
})


test_that("Factors correctly mapped to a positional axis", {
  x <- factor(c(1, 2, 4, 8, 16, 32))
  p <- plot_ly(x = x, y = c(1, 2, 3, 4, 5, 6)) %>% add_markers()
  l <- expect_traces(p, 1, "factor-axis")
  expect_equivalent(l$layout$xaxis$type, "category")
  expect_equivalent(l$layout$xaxis$categoryorder, "array")
  expect_equivalent(l$layout$xaxis$categoryarray, levels(x))
})

test_that("Character strings correctly mapped to a positional axis", {
  # scramble alphabet order
  letters <- LETTERS[as.numeric(sort(as.character(1:26)))]
  p <- plot_ly(x = letters, y = seq_along(letters)) %>% 
    add_bars(color = rep(c("a1", "a2"), length.out = 26))
  l <- expect_traces(p, 2, "character-axis")
  expect_equivalent(l$layout$xaxis$type, "category")
  expect_equivalent(l$layout$xaxis$categoryorder, "array")
  expect_equivalent(l$layout$xaxis$categoryarray, LETTERS)
})

test_that("Histogram", {
  p <- plot_ly(x = rnorm(100))
  l <- expect_traces(p, 1, "histogram")
  o <- unlist(lapply(l$data, "[[", "orientation"))
  types <- unlist(lapply(l$data, "[[", "type"))
  expect_null(o)
  expect_equivalent(unique(types), "histogram")
})

test_that("Discrete variable mapped to x creates horizontal bar chart", {
  p <- plot_ly(y = rnorm(100))
  l <- expect_traces(p, 1, "histogram-vert")
  o <- unlist(lapply(l$data, "[[", "orientation"))
  types <- unlist(lapply(l$data, "[[", "type"))
  expect_equivalent(unique(o), "h")
  expect_equivalent(unique(types), "histogram")
})

test_that("Can avoid inheriting attributes", {
  p <- plot_ly(mtcars, x = ~wt, y = ~mpg, color = I("red")) %>%
    add_histogram(x = ~factor(vs), inherit = FALSE)
  l <- expect_traces(p, 1, "inherit-FALSE")
  expect_equivalent(l$data[[1]][["type"]], "histogram")
  expect_equivalent(l$data[[1]][["x"]], factor(mtcars[["vs"]]))
  expect_null(l$data[[1]][["y"]])
  expect_true(l$data[[1]][["marker"]][["color"]] != toRGB("red"))
})

test_that("Complex example works", {
  # note how median (the variable) doesn't exist in the second layer 
  p <- txhousing %>%
    plot_ly(x = ~date, y = ~median) %>%
    group_by(city) %>%
    add_lines(alpha = 0.2, name = "Texan Cities", hoverinfo = "none") %>%
    group_by(date) %>% 
    summarise(
      q1 = quantile(median, 0.25, na.rm = TRUE),
      m = median(median, na.rm = TRUE),
      q3 = quantile(median, 0.75, na.rm = TRUE)
    ) %>%
    add_lines(y = ~m, color = I("red"), name = "median") %>%
    add_ribbons(ymin = ~q1, ymax = ~q3, color = I("red"), name = "IQR")
  
  l <- expect_traces(p, 3, "time-series-summary")
})


test_that("span/size controls errorbar thickness/width", {
  
  p <- plot_ly(x = 1:10, y = 1:10, error_x = list(value = 3), error_y = list(value = 2), span = I(5), size = I(10), stroke = I("black"), color = I("red")) %>%
    plotly_build()
  
  d <- p$x$data
  expect_length(d, 1)
  
  expect_true(d[[1]]$error_x$value == 3)
  expect_true(d[[1]]$error_x$thickness == 5)
  expect_true(d[[1]]$error_x$width == 10)
  expect_true(d[[1]]$error_x$color == toRGB("red"))
  
  expect_true(d[[1]]$error_y$value == 2)
  expect_true(d[[1]]$error_y$thickness == 5)
  expect_true(d[[1]]$error_y$width == 10)
  expect_true(d[[1]]$error_y$color == toRGB("red"))
})


test_that("Vector of redundant text is reduced to string when hoveron=fills", {
  
  # see https://github.com/ropensci/plotly/issues/1233
  d <- data.frame(
    AA = c(2,3,3,2, NA, 6,7,7,6, NA),
    BB = c(2,2,3,2, NA, 6,6,7,6, NA),
    CC = c(rep('abc', 5), rep('xyz', 5)),
    LL = c(rep('A', 5), rep('B', 5))
  )
  
  
  p <- plot_ly(d) %>%
    add_trace(x = ~AA,
              y = ~BB,
              text = ~paste('<br> <b>Example</b> of <em>custom</em> hover text <br>', LL, '<br>', CC, '<br>.'),
              split = ~LL, 
              mode = "lines", 
              fill = "toself", 
              hoveron = 'fills',
              type = "scatter", 
              color = I(c(rep(toRGB("black", 1), 5),
                          rep(toRGB("red", 1), 5)))
    )
  
  b <- plotly_build(p)
  d <- b$x$data
  expect_length(d, 2)
  expect_true(d[[1]]$line$color == toRGB("black"))
  expect_true(d[[1]]$fillcolor == toRGB("black", 0.5))
  expect_true(d[[2]]$line$color == toRGB("red"))
  expect_true(d[[2]]$fillcolor == toRGB("red", 0.5))
  expect_true(
    d[[1]]$text == '<br> <b>Example</b> of <em>custom</em> hover text <br> A <br> abc <br>.'
  )
  expect_true(
    d[[2]]$text == '<br> <b>Example</b> of <em>custom</em> hover text <br> B <br> xyz <br>.'
  )
})


test_that("Can map data to legendgroup", {
  d <- data.frame(
    x = 1:100,
    y = runif(100),
    group = letters[1:5]
  )
  
  l <- plot_ly(data = d, x = ~x, y = ~y) %>%
    add_bars(color = ~group,  legendgroup = ~group) %>%
    add_markers(color = ~group, legendgroup = ~group) %>%
    plotly_build()
  
  expect_length(l$x$data, 10)
  
  markers <- compact(lapply(l$x$data, function(tr) if (tr$type == "scatter") tr))
  for (i in seq_along(markers)) {
    expect_length(markers[[i]]$legendgroup, 1)
    expect_true(markers[[i]]$legendgroup == letters[[i]])
  }
  
  bars <- compact(lapply(l$x$data, function(tr) if (tr$type == "bar") tr))
  for (i in seq_along(bars)) {
    expect_length(bars[[i]]$legendgroup, 1)
    expect_true(bars[[i]]$legendgroup == letters[[i]])
  }
})
