context("shiny")


test_that("event_register() registers an event", {
  p <- plotly_build(plot_ly(x = 1))
  
  expect_false("plotly_selecting" %in% p$x$shinyEvents)
  
  p <- event_register(p, "plotly_selecting")
  
  expect_true("plotly_selecting" %in% p$x$shinyEvents)
})


test_that("event_unregister() de-registers an event", {
  p <- plotly_build(plot_ly(x = 1))
  
  expect_true("plotly_selected" %in% p$x$shinyEvents)
  
  p <- event_unregister(p, "plotly_selected")
  
  expect_false("plotly_selected" %in% p$x$shinyEvents)
})
