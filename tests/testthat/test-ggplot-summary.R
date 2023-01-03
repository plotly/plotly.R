test_that("stat_summary works", {
  p <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
    stat_summary(fun = mean, fun.max = max, fun.min = min) +
    stat_summary(fun = median, geom = "crossbar", linetype = "dashed")

  expect_doppelganger_built(p, "stat-summary") 
})
