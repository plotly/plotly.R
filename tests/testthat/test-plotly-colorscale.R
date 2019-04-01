context("colorscales")


test_that("Can specify marker.colorscale", {
  p <- plot_ly(
    x = c(-9, -6, -5, -3, -1), 
    y = c(0, 1, 4, 5, 7), 
    marker = list(
      color = 1:5, 
      colorscale='Rainbow', 
      showscale = TRUE
    )
  ) 
  l <- expect_doppelganger_built(p, "marker.colorscale")
})

test_that("Can specify contour colorscale", {
  p <- plot_ly(
    x = c(-9, -6, -5, -3, -1), 
    y = c(0, 1, 4, 5, 7), 
    z = matrix(c(10, 10.625, 12.5, 15.625, 20, 5.625, 6.25, 8.125, 11.25, 15.625, 2.5, 3.125, 5, 8.125, 12.5, 0.625, 1.25, 3.125,
                 6.25, 10.625, 0, 0.625, 2.5, 5.625, 10), nrow = 5, ncol = 5), 
    type = "contour",
    colorscale = 'Rainbow'
  )
  l <- expect_doppelganger_built(p, "contour-colorscale")
})

test_that("Can provide a color interpolation function", {
  p <- plot_ly(x = 1:10, y = 1:10, color = 1:10, colors = scales::colour_ramp(c("red", "green")))
  l <- expect_doppelganger_built(p, "colorRamp")
})

test_that("Can specify contour colorscale", {
  
  plot_colorscale <- function(colorscale) {
    plot_ly(
      x = 1:10, 
      y = 1:10, 
      marker = list(
        color = c(1:9, NA), 
        colorscale = colorscale, 
        showscale = TRUE
      )
    )
  }
  
  pal <- scales::colour_ramp(c("red", "green"))
  colorScale <- list(
    val = seq(0, 1, by = 0.1), 
    col = pal(seq(0, 1, by = 0.1))
  )
  test_list   <- plot_colorscale(colorScale)
  test_df     <- plot_colorscale(as.data.frame(colorScale))
  test_matrix <- plot_colorscale(as.matrix(as.data.frame(colorScale)))
  
  expect_doppelganger_built(test_list, "test_list")
  expect_doppelganger_built(test_df, "test_df")
  expect_doppelganger_built(test_matrix, "test_matrix")
  
  test_list_2 <- plot_colorscale(list(list(0, "rgb(0,0,255)"), list(1, "rgb(0,255,0)")))
  test_list_3 <- plot_colorscale(list(list(0, 1), list("rgb(0,0,255)", "rgb(0,255,0)")))
  
  expect_doppelganger_built(test_list_2, "test_list_2")
  expect_doppelganger_built(test_list_3, "test_list_3")
})


test_that("contour colorscale supports alpha", {
  p <- plot_ly(z = volcano, type = "contour", stroke = I("black"), alpha = 0.1)
  l <- expect_doppelganger_built(p, "contour-alpha")
})
