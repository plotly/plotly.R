context("partial-bundles")

test_that("Can reduce saved file size with an basic (auto) partial bundle by half", {
  skip_on_cran()
  
  p1 <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
  p2 <- partial_bundle(p1)
  expect_match(plotlyjsBundle(p2)$name, "basic")
  
  f1 <- tempfile(fileext = ".html")
  f2 <- tempfile(fileext = ".html")
  file_size <- function(p, f) {
    owd <- setwd(dirname(f))
    on.exit(setwd(owd))
    htmlwidgets::saveWidget(p, f)
    file.info(f)$size / 1e6
  }
  expect_true(file_size(p1, f1) / 2 > file_size(p2, f2))
})

test_that("Can find the right bundle", {
  skip_on_cran()
  
  p1 <- plot_ly(z = ~volcano) %>% add_heatmap()
  p2 <- partial_bundle(p1)
  expect_match(plotlyjsBundle(p2)$name, "cartesian")
  
  p3 <- plot_ly(z = ~volcano) %>% add_surface()
  p4 <- partial_bundle(p3)
  expect_match(plotlyjsBundle(p4)$name, "gl3d")
  
  # At least right now, we don't support multiple partial bundles
  expect_warning(
    subplot(p1, p3) %>% partial_bundle(),
    "Using the main (full) bundle", 
    fixed = TRUE
  )
})

test_that("Throws an informative error if wrong bundle is specified", {
  p1 <- plot_ly(z = ~volcano) %>% add_heatmap()
  
  expect_error(
    partial_bundle(p1, type = "basic"),
    "The 'basic' bundle supports the following trace types: 'scatter', 'bar', 'pie'"
  )
})

test_that("Can specify the partial bundle", {
  skip_on_cran()
  
  p1 <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
  p2 <- partial_bundle(p1, type = "basic")
  p3 <- partial_bundle(p1, type = "cartesian")
  
  expect_match(plotlyjsBundle(p2)$name, "basic")
  expect_match(plotlyjsBundle(p3)$name, "cartesian")
  
})
