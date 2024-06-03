
test_that("geom_contour is translated to a path", {
  skip_if_not_installed("reshape2")
  
  volcano3d <- getFromNamespace("melt", "reshape2")(volcano)
  names(volcano3d) <- c("x", "y", "z")
  # Draw a contour plot using geom_contour
  gg <- ggplot(volcano3d) + geom_contour(aes(x=x, y=y, z=z))
  L <- expect_doppelganger_built(gg, "contour")
  expect_equivalent(length(L$data), 1)
  expect_identical(L$data[[1]]$type, "scatter")
  expect_identical(L$data[[1]]$mode, "lines")
})

test_that("raster/contour works with scale_fill_binned", {
  xy_grid <- expand.grid(ps=seq(-1,3, length=100),
    trs=seq(-1,3,length=100))
  xy_grid$ptl <-  xy_grid$ps^2 + xy_grid$trs^2
  
  gg <- ggplot(xy_grid) +
    geom_raster(aes(y=ps,x=trs,fill=ptl), interpolate = TRUE) +
    geom_contour(aes(y=ps,x=trs,z=ptl), breaks = c(2,4,6,8,10), color='black') +
    scale_fill_binned(low = 'red', high = 'green', breaks = c(2,4,6,8,10))
  
  expect_doppelganger_built(gg, "raster-contour-binned")
})
