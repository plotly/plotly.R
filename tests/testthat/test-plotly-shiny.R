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


test_that("event_data shiny app works", {
  skip_on_cran()
  skip_if_not_installed("shinytest")
  if (enable_vdiffr) skip("shiny testing not performed during visual testing")
  
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "plotly", "examples", "shiny", "event_data")
  shinytest::expect_pass(shinytest::testApp(appdir, compareImages = FALSE))
})
