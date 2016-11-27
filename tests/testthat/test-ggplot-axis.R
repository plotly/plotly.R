context("axes")

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
	geom_point() + 
	scale_x_continuous(position="top") + 
	scale_y_continuous(position="right")


test_that("Axis positions move to top and right", {
  info <- save_outputs(p, "axis_position")
  expect_equal(length(info$data), 1)
  expect_identical(info$layout$xaxis$side, "top")
  expect_identical(info$layout$yaxis$side, "right")
  expect_traces(p, 1, "traces")
})


p <- ggplot(mtcars, aes(x=mpg, y=wt)) + 
	geom_point()

test_that("Axis positions stay at bottom and left", {
  info <- save_outputs(p, "axis_position")
  expect_equal(length(info$data), 1)
  expect_identical(info$layout$xaxis$side, "bottom")
  expect_identical(info$layout$yaxis$side, "left")
  expect_traces(p, 1, "traces")
})