context("Axis moving")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("axis-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

p <- ggplot(mtcars, aes(x=mpg, y=wt)) + 
	geom_point()

# p3 <- p + facet_wrap(~carb)

test_that("Axis position moves to top", {
	p <- p + scale_x_continuous(position="top")
  
  info <- save_outputs(p, "axis_move_top")
  expect_equal(length(info$data), 1)
  expect_identical(info$layout$xaxis$side, "top")
})

test_that("Axis position moves to right", {
  p <- p + scale_y_continuous(position="right")
  
  info <- save_outputs(p, "axis_move_right")
  expect_equal(length(info$data), 1)
  expect_identical(info$layout$yaxis$side, "right")
})

test_that("Axis position moves to top (facets)", {
  p <- p + scale_x_continuous(position="top") + facet_wrap(~carb)

  info <- save_outputs(p, "axis_move_top_facet")
  expect_equal(length(info$data), 6)
  expect_equal(info$layout$xaxis$anchor, "y1")
  expect_identical(info$layout$xaxis$side, "top")
})

test_that("Axis position moves to top (facets)", {
  p <- p + scale_y_continuous(position="right") + facet_wrap(~carb)

  info <- save_outputs(p, "axis_move_right_facet")
  
  expect_equal(length(info$data), 6)

  expect_equal(info$layout$yaxis$anchor, "x3")
  expect_identical(info$layout$yaxis$side, "right")
})

test_that("Axis positions stay at bottom and left", {
  info <- save_outputs(p, "axis_stay")
  
  expect_equal(length(info$data), 1)
  
  expect_identical(info$layout$xaxis$side, "bottom")
  expect_identical(info$layout$yaxis$side, "left")

  expect_equal(info$layout$xaxis$anchor, "y")
  expect_equal(info$layout$yaxis$anchor, "x")
})


test_that("Axis positions stay at bottom and left (facet)", {
  p <- p + facet_wrap(~carb)
  info <- save_outputs(p, "axis_stay_facet")
  
  expect_equal(length(info$data), 6)
  
  expect_identical(info$layout$xaxis$side, "bottom")
  expect_identical(info$layout$yaxis$side, "left")

  expect_equal(info$layout$xaxis$anchor, "y2")
  expect_equal(info$layout$yaxis$anchor, "x")
})

