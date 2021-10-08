test_that("add_area() works", {
  data(wind)
  p <- plot_ly(wind, r = ~r, t = ~t) %>% 
    add_area(color = ~nms) %>%
    layout(radialaxis = list(ticksuffix = "%"), orientation = 270)
  expect_doppelganger_built(p, "add-area")
})
