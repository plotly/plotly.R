# partial_bundle() is plotly.core's: the plot loads the smallest plotly.js
# partial bundle with its trace types
plotlyjs <- function(p) {
  deps <- Filter(function(d) identical(d$name, "plotly-main"), as_widget(p)$dependencies)
  deps[[1]]$script
}

test_that("the full plotly.js bundle is used by default", {
  p <- plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "markers")
  expect_identical(plotlyjs(p), "plotly.min.js")
})

test_that("partial_bundle() picks the right bundle", {
  p1 <- plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "markers")
  expect_identical(plotlyjs(partial_bundle(p1)), "plotly-basic.min.js")
  p2 <- plot_ly(z = ~volcano) %>% add_heatmap()
  expect_identical(plotlyjs(partial_bundle(p2)), "plotly-cartesian.min.js")
  p3 <- plot_ly(z = ~volcano) %>% add_surface()
  expect_identical(plotlyjs(partial_bundle(p3)), "plotly-gl3d.min.js")
  # ggplotly() plots too
  g <- ggplotly(ggplot(mtcars, aes(wt, mpg)) + geom_point())
  expect_identical(plotlyjs(partial_bundle(g)), "plotly-basic.min.js")
})

test_that("a named bundle without the plot's trace types falls back to the full one", {
  p <- plot_ly(z = ~volcano) %>% add_surface()
  expect_warning(js <- plotlyjs(partial_bundle(p, type = "basic")), "surface")
  expect_identical(js, "plotly.min.js")
})
