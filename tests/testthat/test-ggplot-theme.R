context("ggplot themes")

iris.base <- ggplot(iris) + geom_point(aes(Petal.Width, Sepal.Width))

test_that("background translated correctly",{
  ggiris <- iris.base + theme(panel.background=element_rect(fill="blue")) +
    theme(plot.background=element_rect(fill="green"))
  info <- gg2list(ggiris)
  L <- info$kwargs$layout
  expect_identical(L$plot_bgcolor, toRGB("blue"))
  expect_identical(L$paper_bgcolor, toRGB("green"))
})

test_that("grid/ticks translated correctly",{
  ggiris <- iris.base + theme(axis.ticks=element_line(colour="red")) +
    theme(panel.grid.major=element_line(colour="violet"))
  info <- gg2list(ggiris)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$tickcolor, toRGB("red"))
    expect_identical(ax.list$gridcolor, toRGB("violet"))
  }
})

test_that("dotted/dashed grid translated as line with alpha=0.1",{
  ggiris <- iris.base + theme(panel.grid.major=element_line(linetype="dashed"))
  info <- gg2list(ggiris)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$gridcolor, toRGB("white", 0.1))
  }
})

test_that("plot panel border is translated correctly", {
  ggiris <- iris.base + theme_grey() # has no panel.border
  info <- gg2list(ggiris)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$showline, FALSE)
  }
  red <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    theme(panel.border=element_rect(colour="red"))
  info <- gg2list(red)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$showline, TRUE)
    expect_identical(ax.list$linecolor, toRGB("red"))
  }
})
