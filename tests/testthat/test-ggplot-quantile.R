context("quantile")

test_that("Basic geom_quantile() works", {
  
  p <- ggplot(mpg, aes(displ, 1 / hwy)) + 
    geom_point() +
    geom_quantile()
  
  l <- plotly_build(p)$x
  
  expect_length(l$data, 4)
  
  for (i in 2:4) {
    tr <- l$data[[i]]
    expect_equal(tr$type, "scatter")
    expect_equal(tr$mode, "lines")
    expect_equal(
      tr$line$color, toRGB(GeomQuantile$default_aes[["colour"]])
    )
  }
  
})

test_that("Can specify gpar() in geom_quantile()", {
  
  # TODO: implement lineend/linejoin/linemitre?
  
  p <- ggplot(mpg, aes(displ, 1 / hwy)) + 
    geom_point() +
    geom_quantile(colour = "red", alpha = 0.5)
  
  l <- plotly_build(p)$x
  
  expect_length(l$data, 4)
  
  for (i in 2:4) {
    tr <- l$data[[i]]
    expect_equal(tr$type, "scatter")
    expect_equal(tr$mode, "lines")
    expect_equal(
      tr$line$color, toRGB("red", 0.5)
    )
  }
  
  
})

