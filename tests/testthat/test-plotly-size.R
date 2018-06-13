context("size")

test_that("sizemode is always respected", {
  
  # https://github.com/ropensci/plotly/issues/1133
  p <- plot_ly(mtcars, x = ~wt, y = ~wt, size = ~wt, color = ~as.factor(carb)) 
  d <- plotly_build(p)$x$data
  expect_length(d, length(unique(mtcars$carb)))
  
  for (i in seq_along(d)) {
    expect_true(d[[i]]$type == "scatter")
    expect_true(d[[i]]$mode == "markers")
    expect_true(d[[i]]$marker$sizemode == "area")
    # Make sure size is always translated as an array in this case, because plotly.js
    # https://github.com/plotly/plotly.js/issues/2735
    s <- d[[i]]$marker$size
    if (length(s) == 1) expect_is(s, "AsIs")
  }
  
})
