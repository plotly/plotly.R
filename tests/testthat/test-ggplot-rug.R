context("rug")


base <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

test_that("Basic geom_rug() works", {
  
  p <- base + geom_rug(sides = "b")
  l <- plotly_build(p)$x
  expect_length(l$data, 2)
  expect_equivalent(l$data[[2]]$mode, "lines")
  
  # default should be "bl" (bottom-left)
  p <- base + geom_rug()
  l <- plotly_build(p)$x
  expect_length(l$data, 3)
  for (i in 2:3) {
    expect_equivalent(l$data[[i]]$mode, "lines")
  }
  
  p <- base + geom_rug(sides = "trbl")
  l <- plotly_build(p)$x
  expect_length(l$data, 5)
  for (i in 2:5) {
    expect_equivalent(l$data[[i]]$mode, "lines")
  }
  
})

test_that("geom_rug() with facets", {
  
  p <- base + geom_rug() +    
    facet_wrap(~vs, scales = "free")
  l <- plotly_build(p)$x
  expect_length(l$data, 6)
  
})

base <- base + facet_wrap(~vs, scales = "free")

test_that("geom_rug() with graphical parameters", {
  
  p <- base + 
    geom_rug(alpha = 0.5, color = "red", linetype = 2) + 
    facet_wrap(~vs, scales = "free_y")
    
  l <- plotly_build(p)$x
  expect_length(l$data, 6)
  
  modes <- sapply(l$data, "[[", "mode")
  rug <- l$data[modes %in% "lines"]
  for (i in seq_along(rug)) {
    expect_equivalent(rug[[i]]$line$color, toRGB("red", 0.5))
    expect_equivalent(rug[[i]]$line$dash, lty2dash(2))
  }
  
})
