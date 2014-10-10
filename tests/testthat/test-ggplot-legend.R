context("legends")

test_that("legend can be hidden", {
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width, color=Species))+
    theme(legend.position="none")
  info <- gg2list(ggiris)
  expect_identical(info$kwargs$layout$showlegend, FALSE)

  save_outputs(ggiris, "legend-hidden")
})

test_that("legend entries appear in the correct order", {
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width, color=Species))
  getnames <- function(L){
    traces <- L[names(L)==""]
    expect_equal(length(traces), 3)
    as.character(sapply(traces, "[[", "name"))
  }
  info <- gg2list(ggiris)
  ## Default is the same as factor levels.
  expect_identical(getnames(info), levels(iris$Species))
  ## Custom breaks should be respected.
  breaks <- c("versicolor", "setosa", "virginica")
  ggbreaks <- ggiris+scale_color_discrete(breaks=breaks)
  info.breaks <- gg2list(ggbreaks)
  expect_identical(getnames(info.breaks), breaks)

  save_outputs(ggiris, "legend-order")
})
