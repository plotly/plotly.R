context("ggplot-api")


test_that("summarise_layout() gives the expected summary", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(drv ~ cyl)
  built <- ggplot_build(p)
  layout <- summarise_layout(built)
  
  expect_true(nrow(layout) == 9)
  expect_equal(lengths(layout$vars), rep(2, 9))
  # we access most (if not all) of these variables in the summary
  expect_length(
    setdiff(
      c('panel', 'row', 'col', 'vars', 'xmin', 'xmax', 'ymin', 'ymax', 'xscale', 'yscale'),
      names(layout)
    ), 0
  )
  
})
