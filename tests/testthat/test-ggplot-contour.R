context("Contour")



test_that("geom_contour is translated to a path", {
  skip_if_not_installed("reshape2")
  
  volcano3d <- getFromNamespace("melt", "reshape2")(volcano)
  names(volcano3d) <- c("x", "y", "z")
  # Draw a contour plot using geom_contour
  gg <- ggplot(volcano3d) + geom_contour(aes(x=x, y=y, z=z))
  L <- save_outputs(gg, "contour")
  expect_equivalent(length(L$data), 1)
  expect_identical(L$data[[1]]$type, "scatter")
  expect_identical(L$data[[1]]$mode, "lines")
})


