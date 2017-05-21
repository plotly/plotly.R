context("annotation_")


# Generate data
rainbow <- matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50)
p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() + facet_wrap(~vs) +
  annotation_raster(rainbow, 15, 20, 3, 4)


test_that("Basic annotation_raster() example works", {
  
  l <- plotly_build(p)$x
  
  # currently we add a blank geom for each raster
  # not sure if that is the best thing to do...
  expect_length(l$data, 4)
  
  expect_length(l$layout$images, 2)
  expect_equivalent(l$layout$images[[1]]$xref, "x") 
  expect_equivalent(l$layout$images[[1]]$yref, "y") 
  expect_equivalent(l$layout$images[[2]]$xref, "x2") 
  expect_equivalent(l$layout$images[[2]]$yref, "y")
  
  for (i in 1:2) {
    expect_equivalent(l$layout$images[[i]]$layer, "above")
    expect_equivalent(l$layout$images[[i]]$xanchor, "left")
    expect_equivalent(l$layout$images[[i]]$yanchor, "bottom")
    expect_equivalent(l$layout$images[[i]]$sizex, 5)
    expect_equivalent(l$layout$images[[i]]$sizey, 1)
    expect_equivalent(l$layout$images[[i]][["x"]], 15)
    expect_equivalent(l$layout$images[[i]][["y"]], 3)
    expect_equivalent(l$layout$images[[i]]$sizing, "stretch")
  }
  
  # TODO: how to test the data URI content?
  
})



usamap <- map_data("state")
seal.sub <- subset(seals, long > -130 & lat < 45 & lat > 40)
p <- ggplot(seal.sub, aes(x = long, y = lat)) +
  annotation_map(usamap, fill = "NA", colour = "grey50") +
  geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat))


test_that("Basic annotation_map() example works", {
  
  l <- plotly_build(p)$x
  
  expect_equivalent(l$data[[1]]$type, "scatter")
  expect_equivalent(l$data[[1]]$fill, "toself")
  expect_equivalent(l$data[[1]]$fillcolor, "transparent")
  expect_equivalent(l$data[[1]]$hoveron, "fills")
  expect_false(l$data[[1]]$showlegend)
  
})
