context("partial-bundles")


test_that("Can reduce saved file size with an auto partial bundle", {
  
  p1 <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
  p2 <- partial_bundle(p1)
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
