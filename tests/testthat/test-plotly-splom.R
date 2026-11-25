


test_that("No cartesian axes are supplied to a splom chart", {
  
  p <- plot_ly(
    type = 'splom',
    dimensions = list(
      list(values=c(1,2,3), label="A"),
      list(values=c(2,5,6), label="B")
    )
  )
  expect_false(any(c("xaxis", "yaxis") %in% names(plotly_build(p)$x$layout)))
})

test_that("numeric colors use a splom marker colorbar", {
  values <- c(1, 3, 5)
  p <- plot_ly(
    type = "splom",
    dimensions = list(
      list(values = c(1, 2, 3), label = "A"),
      list(values = c(2, 5, 6), label = "B")
    ),
    color = values
  )

  expect_warning(p <- plotly_build(p), NA)
  built <- p$x
  expect_length(built$data, 1)
  expect_false(any(c("xaxis", "yaxis") %in% names(built$layout)))
  expect_equal(as.numeric(built$data[[1]]$marker$color), values)
  expect_true(as.logical(built$data[[1]]$marker$showscale))

  modified <- colorbar(p, title = "Score", limits = c(2, 4))$x$data[[1]]$marker
  expect_equal(modified$colorbar$title, "Score")
  expect_equal(as.numeric(modified$color), c(NA, 3, NA))
  expect_false(as.logical(hide_colorbar(p)$x$data[[1]]$marker$showscale))
})

test_that("multiple splom traces share a numeric scale and retain their legends", {
  dimensions <- list(
    list(values = c(1, 2, 3), label = "A"),
    list(values = c(2, 5, 6), label = "B")
  )
  p <- plot_ly(
    type = "splom", dimensions = dimensions, color = rep(1, 3), name = "First",
    marker = list(showscale = FALSE)
  )
  p <- add_trace(
    p, type = "splom", dimensions = dimensions, color = 5, name = "Second",
    marker = list(colorbar = list(title = "Shared score")), inherit = FALSE
  )

  expect_warning(built <- plotly_build(p)$x, NA)
  expect_equal(vapply(built$data, `[[`, character(1), "type"), rep("splom", 2))
  expect_true(as.logical(built$layout$showlegend))

  first <- built$data[[1]]$marker
  second <- built$data[[2]]$marker
  expect_false(first$showscale)
  expect_true(as.logical(second$showscale))
  expect_equal(second$colorbar$title, "Shared score")
  expect_equal(as.numeric(first$color), rep(1, 3))
  expect_equal(as.numeric(second$color), rep(5, 3))
  expect_equal(first$colorscale, second$colorscale)
  expect_equal(as.numeric(c(first$cmin, first$cmax)), c(1, 5))
  expect_equal(as.numeric(c(second$cmin, second$cmax)), c(1, 5))
})
