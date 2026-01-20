df = data.frame(width = 1:3, height = 1:3, col = letters[1:3])
test_that("ggplotly automatically converts `color` aes to `colour`", {
    p <- qplot(width, height,
                        data = df, color = col)
    # color variable is not shown
    color <- plotly_build(ggplotly(p, tooltip = c("color")))
    # colour (with u!) variable is shown
    expect_identical(color$x$data, plotly_build(ggplotly(p, tooltip = c("colour")))$x$data)
})

test_that("scale_*_manual with unused aesthetics does not error (#2466)", {
  # scale has aesthetics = c('colour', 'fill') but plot only uses 'colour'
  p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
    geom_point() +
    scale_colour_manual(
      values = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green"),
      aesthetics = c("colour", "fill")
    )
  # Should not error with "undefined columns selected"
  expect_error(plotly_build(ggplotly(p)), NA)
})

test_that("multi-aesthetic scales show legend and split traces correctly (#2467)", {
  # When scale has aesthetics = c('colour', 'fill') and both are used,
  # traces should be split by the variable and legend should appear
  p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species, fill = Species)) +
    geom_point(shape = 21) +
    scale_colour_manual(
      values = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green"),
      aesthetics = c("colour", "fill")
    )
  L <- plotly_build(ggplotly(p))$x
  # Should have 3 traces (one per Species level)
  expect_equal(length(L$data), 3)
  # Should show legend
  expect_true(any(sapply(L$data, function(d) isTRUE(d$showlegend))))
  # Trace names should be the species names
  trace_names <- sapply(L$data, function(d) d$name)
  expect_true(all(c("setosa", "versicolor", "virginica") %in% trace_names))
})

