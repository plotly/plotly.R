context("image-trace")

test_that("image trace renders", {
  plot_ly() %>%
    add_trace(
      type = "image",
      colormodel = "rgb",
      hoverinfo = "all",
      dy = 1,
      dx = 0.5,
      # TODO: do something intelligent with raster objects
      z = list(list(list(255, 0, 0), list(0, 255, 0), list(0, 0, 255)))
    ) %>%
    add_trace(
      type = "image",
      colormodel = "hsl",
      hoverinfo = "all",
      z = list(
        list(list(0, 33, 50), list(0, 66, 50), list(0, 100, 50)),
        list(list(90, 33, 50), list(90, 66, 50), list(90, 100, 50)),
        list(list(180, 33, 50), list(180, 66, 50), list(180, 100, 50)),
        list(list(270, 33, 50), list(270, 66, 50), list(270, 100, 50))
      ),
      xaxis = "x2",
      yaxis = "y2"
    ) %>%
    layout(grid = list(rows = 1, columns = 2, pattern = "independent")) %>%
    expect_doppelganger("colormodel")
})


test_that("add_image() works", {
  plot_ly() %>%
    add_image(
      z = as.raster(matrix(hcl(0, 80, seq(50, 80, 10)), nrow = 4, ncol = 5))
    ) %>%
    expect_doppelganger("raster-basic")
})
