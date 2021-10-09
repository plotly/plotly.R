test_that("add_area() works", {
  data(wind)
  p <- plot_ly(wind, r = ~r, theta = ~t) %>% 
    add_area(color = ~nms) %>%
    layout(
      polar = list(
        radialaxis = list(ticksuffix = "%"), 
        angularaxis = list(rotation = 90)
      )
    )
  expect_doppelganger_built(p, "add-area")
})
