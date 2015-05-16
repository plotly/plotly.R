context("Fixed coordinates")

# Expect trace function
expect_traces <- function(gg, n_traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("coord_fixed-", name))
  L <- gg2list(gg)
  all_traces <- L$data
  no_data <- sapply(all_traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has_data <- all_traces[!no_data]
  expect_equal(length(has_data), n_traces)
  list(traces = has_data, layout = L$layout)
}

# Data where x ranges from 0-10, y ranges from 0-30
set.seed(202)
dat <- data.frame(xval = runif(40,0,10), yval = runif(40,0,30))

# Force equal scaling
p <- ggplot(dat, aes(xval, yval)) + geom_point() + coord_fixed()
# Test 
test_that("coord_fixed() is translated to the right height-width ratio", {
  info <- expect_traces(p, 1, "force_equal_scaling")
  tr <- info$traces[[1]]
  la <- info$layout
  expect_identical(tr$type, "scatter")
  # height-width ratio check
  built <- ggplot_build2(p)
  x_range <- range(built[[2]]$ranges[[1]]$x.major_source, na.rm = TRUE)
  y_range <- range(built[[2]]$ranges[[1]]$y.major_source, na.rm = TRUE)
  yx_ratio <- (y_range[2] - y_range[1]) / (x_range[2] - x_range[1])
  expect_identical(la$height/la$width, yx_ratio * p$coordinates$ratio)
})

# Equal scaling, with each 1 on the x axis the same length as y on x axis
p <- ggplot(dat, aes(xval, yval)) + geom_point() + coord_fixed(1/3)
# Test 
test_that("coord_fixed() is translated to the right height-width ratio", {
  info <- expect_traces(p, 1, "force_equal_scaling")
  tr <- info$traces[[1]]
  la <- info$layout
  expect_identical(tr$type, "scatter")
  # height-width ratio check
  built <- ggplot_build2(p)
  x_range <- range(built[[2]]$ranges[[1]]$x.major_source, na.rm = TRUE)
  y_range <- range(built[[2]]$ranges[[1]]$y.major_source, na.rm = TRUE)
  yx_ratio <- (y_range[2] - y_range[1]) / (x_range[2] - x_range[1])
  expect_identical(la$height/la$width, yx_ratio * p$coordinates$ratio)
})