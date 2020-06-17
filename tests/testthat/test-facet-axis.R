test_that("ggplotly does not break discrete x-axis with facet_yyyy in panels > 1 with only one category", {
  d <- data.frame(cat = c("A", "A", "A"), pan = paste("Panel", c(1, 2, 2)))
  gp <- ggplot(d, aes(cat)) +
    geom_bar() +
    facet_wrap(~pan)
  L <- plotly_build(ggplotly(gp))
  # tickvals, ticktext and categoryarray have class 'AsIs'
  lapply(L$x$layout[c("xaxis", "xaxis2")], function(axis) {
    expect_s3_class(axis$tickvals, "AsIs")
    expect_s3_class(axis$ticktext, "AsIs")
    expect_true(axis$ticktext == "A")
    expect_s3_class(axis$categoryarray, "AsIs")
    expect_true(axis$categoryarray == "A")
  })
})
