context("fixed coordinates")

p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() + coord_fixed()

test_that("ggplot2::coord_fixed() works", {
  #TODO: what is a good test?
  expect_true(FALSE)
})

# TODO: create plotly::coord_fixed() and implement (leverage HTMLwidget's resize method!)
p <- plot_ly(mtcars, x = mpg, y = wt, mode = "markers") %>%
  coord_fixed()

test_that("plotly::coord_fixed() works", {
  #TODO: what is a good test?
  expect_true(FALSE)
})
