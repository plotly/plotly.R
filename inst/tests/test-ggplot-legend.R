context("legends")

test_that("legend can be hidden", {
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width, color=Species))+
    theme(legend.position="none")
  info <- gg2list(ggiris)
  expect_identical(info$kwargs$layout$showlegend, FALSE)
})
