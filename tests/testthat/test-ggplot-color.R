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
  # (Note: trace splitting with multi-aesthetic scales is a separate issue #2467)
  expect_error(plotly_build(ggplotly(p)), NA)
})

