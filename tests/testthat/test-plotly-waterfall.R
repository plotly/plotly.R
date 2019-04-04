context("waterfall")

test_that("Simple waterfall works", {
  p <- plot_ly() %>%
    add_trace(
      type = "waterfall",
      x = c(0, 1, 2, 3, 4, 5),
      y = c(1, 0.5, 0.7, -1.2, 0.3, 0.4)
    )
  
  expect_doppelganger_built(p, "waterfall-simple")
})


