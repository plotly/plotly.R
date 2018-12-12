context("customdata")

# TODO: use shinytest to make sure we can access the right value in shiny
test_that("ggplotly relays customdata", {
  nms <- row.names(mtcars)
  p <- ggplot(mtcars, aes(x = mpg, y = wt, customdata = nms)) + geom_point()
  l <- plotly_build(p)
  trace <- l$x$data[[1]]
  expect_equivalent(trace$customdata, nms)
})
