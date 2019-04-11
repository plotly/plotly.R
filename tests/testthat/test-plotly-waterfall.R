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

test_that("Waterfall missing values are retained", {
  y <- c("Sales", "Consulting", "Maintenance", "Other revenue", "Net revenue", "Purchases", "Material expenses", "Personnel expenses", "Other expenses", "Operating profit", "Investment income", "Financial income", "Profit before tax", "Income tax (15%)", "Profit after tax")
  
  d <- data.frame(
    measure = c("relative", "relative", "relative", "relative", "total", "relative", "relative", "relative", "relative", "total", "relative", "relative", "total", "relative", "total"),
    y = factor(y, levels = y),
    x = c(375, 128, 78, 27, NA, -327, -12, -78, -12, NA, 32, 89, NA, -45, NA)
  )
  
  p <- plot_ly(d, measure = ~measure, y = ~y, x = ~x) %>%
    add_trace(type = "waterfall", orientation = "h") %>%
    layout(yaxis = list(autorange = "reversed"))
  
  expect_doppelganger_built(p, "waterfall-missing-values")
})


