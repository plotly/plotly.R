context("isosurface")

# get the values/x/y/z from the plotly.js mock this test is based on
mock <- jsonlite::fromJSON(
  "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/gl3d_isosurface_log-axis_slices_surface-fill.json", 
  simplifyVector = FALSE
)

test_that("isosurface works with colorscale", {
  p <- plot_ly(
    type = "isosurface",
    colorscale = "Rainbow",
    reversescale = FALSE,
    surface = list(show = TRUE, fill = 1),
    spaceframe = list(show = TRUE),
    slices = list(
      x = list(
        show = TRUE, 
        fill = 1, 
        locations = c(-0.9, -0.6, 0)
      ),
      y = list(
        show = TRUE, 
        fill = 1, 
        locations = c(-0.9, -0.6, 0)
      ),
      z = list(
        show = TRUE, 
        fill = 1, 
        locations = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1)
      )
    ),
    caps = list(
      x = list(show = FALSE),
      y = list(show = FALSE),
      z = list(show = FALSE)
    ),
    contour = list(
      show = TRUE,
      width = 4
    ),
    isomin = 200,
    isomax = 500,
    value = mock$data[[1]]$value,
    x = mock$data[[1]]$x,
    y = mock$data[[1]]$y,
    z = mock$data[[1]]$z,
    lighting = list(
      ambient = 0.5,
      diffuse = 1.0,
      specular = 0.75,
      roughness = 1.0,
      fresnel = 0.25
    ),
    lightposition = list(
      x = 10000,
      y = 10000,
      z = 0
    )
  )
  
  expect_doppelganger(p, "isosurface-simple")
})
