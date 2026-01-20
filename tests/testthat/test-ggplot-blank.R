
test_that("geom_blank", {
  skip_if_not_installed("ggplot2", "3.4.0")
  qp <- expect_warning(qplot(), "deprecated")
  l <- ggplotly(qp)$x

  expect_length(l$data, 1)
  expect_false(l$data[[1]]$visible)

  l <- ggplotly(ggplot())$x

  expect_length(l$data, 1)
  expect_false(l$data[[1]]$visible)

})

test_that("geom_blank does not drop legend (#2281)", {
  # When geom_blank() is combined with other geoms, legend should still appear
  p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
    geom_blank() +
    geom_point()

  L <- plotly_build(ggplotly(p))$x

  # geom_point should create 3 visible traces with legend
  visible_traces <- Filter(function(d) !isFALSE(d$visible) && d$type == "scatter", L$data)
  expect_equal(length(visible_traces), 3)
  expect_true(any(sapply(visible_traces, function(d) isTRUE(d$showlegend))))

  # Trace names should be species names
  trace_names <- sapply(visible_traces, function(d) d$name)
  expect_true(all(c("setosa", "versicolor", "virginica") %in% trace_names))
})
