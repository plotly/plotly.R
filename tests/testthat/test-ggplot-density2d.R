context("Density2d")

# Draw a 2d density estimation using geom_density2d
m <- ggplot(MASS::geyser, aes(x=duration, y=waiting)) + 
  geom_point(alpha = 0.4) +
  geom_density2d()
L <- save_outputs(m, "density2d")

test_that("geom_density2d translates to path(s)", {
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[2]]$type, "scatter")
  expect_identical(L$data[[2]]$mode, "lines")
})

faithful$col <- factor(sample(1:20, nrow(faithful), replace = T))
m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  geom_point(aes(colour = col)) +
  xlim(0.5, 6) + ylim(40, 110)

L <- save_outputs(m, "density2dfill")

test_that("StatDensity2d with GeomPolygon translates to filled path(s)", {
  # only the marker traces should be shown in the legend
  legends <- unlist(lapply(L$data, "[[", "showlegend"))
  points <- L$data[legends]
  # make sure we have 20 traces of points
  expect_equivalent(length(points), 20)
  expect_identical(
    unique(unlist(lapply(points, "[[", "type"))), "scatter"
  )
  expect_identical(
    unique(unlist(lapply(points, "[[", "mode"))), "markers"
  )
  # the other traces should be the colorbar and polygons
  notPoints <- L$data[!legends]
  polygons <- notPoints[-length(notPoints)]
  colorbar <- notPoints[[length(notPoints)]]
  expect_identical(
    unique(unlist(lapply(polygons, "[[", "type"))), "scatter"
  )
  expect_identical(
    unique(unlist(lapply(polygons, "[[", "mode"))), "lines"
  )
  expect_identical(
    unique(unlist(lapply(polygons, "[[", "fill"))), "toself"
  )
  # split on fill for polygons 
  # (you can't have two polygons with different fill in a single trace)
  expect_true(
    length(unique(unlist(lapply(polygons, "[[", "fillcolor")))) > 1
  )
  # ensure the legend/guide are placed correctly
  expect_true(L$layout$legend$y < 0.5)
  expect_true(L$layout$legend$yanchor == "top")
  expect_true(colorbar$marker$colorbar$y == 1)
  expect_true(colorbar$marker$colorbar$yanchor == "top")
  expect_true(colorbar$marker$colorbar$len == 0.5)
  
  #test some properties that shouldn't be sensitive to ggplot2 defaults
  expect_true(colorbar$marker$colorbar$title == "level")
  
  # are the hidden colorbar markers on the correct range?
  for (xy in c("x", "y")) {
    rng <- L$layout[[paste0(xy, "axis")]]$range
    expect_true(
      all(min(rng) <= colorbar[[xy]] & colorbar[[xy]] <= max(rng))
    )
  }
  
})

