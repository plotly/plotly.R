test_that("ggplotly does not break discrete x-axis with facet_yyyy in panels > 1 with only one category", {
  d <- data.frame(cat = c("A", "A", "A"), pan = paste("Panel", c(1, 2, 2)))
  gp <- ggplot(d, aes(cat)) +
    geom_bar() +
    facet_wrap(~pan)
  L <- plotly_build(ggplotly(gp))
  # tickvals, ticktext and categoryarray have class 'AsIs'
  expect_equal(class(L$x$layout$xaxis2$tickvals), "AsIs")
  expect_equal(class(L$x$layout$xaxis2$ticktext), "AsIs")
  expect_equal(class(L$x$layout$xaxis2$categoryarray), "AsIs")
})
