context("Build function")

gg <- ggplot(Orange, aes(x=age, y=circumference)) +
  geom_line() +
  facet_wrap(~Tree)

L <- plotly::ggplot_build2(gg)

test_that("ggplot_build2 returns prestats.data", {
  expect_equal(length(L), 4)
  expect_true("prestats.data" %in% names(L))
})

test_that("prestats.data gives the right panel info", {
  gr <- as.integer(L$prestats.data[[1]]$group)
  pa <- as.integer(L$prestats.data[[1]]$PANEL)
  expect_identical(gr, pa)
})
