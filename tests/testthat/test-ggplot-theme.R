context("ggplot themes")

iris.base <- ggplot(iris) +
  geom_point(aes(Petal.Width, Sepal.Width))+
  theme_grey()

test_that("background translated correctly",{
  ggiris <- iris.base + theme(panel.background=element_rect(fill="blue")) +
    theme(plot.background=element_rect(fill="green"))
  info <- gg2list(ggiris)
  L <- info$kwargs$layout
  expect_identical(L$plot_bgcolor, toRGB("blue"))
  expect_identical(L$paper_bgcolor, toRGB("green"))

  save_outputs(ggiris, "theme-background")
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

  save_outputs(ggiris, "theme-ticks-and-grids")
})

test_that("show ticks as 'outside' by default", {
  ggiris <- iris.base
  info <- gg2list(ggiris)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$ticks, "outside")
  }
  
  save_outputs(ggiris, "theme-ticks-default")
})

test_that("do not show zeroline by default", {
  ggiris <- iris.base
  info <- gg2list(ggiris)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$zeroline, FALSE)
  }
  
  save_outputs(ggiris, "theme-zeroline-default")
})

test_that("dotted/dashed grid translated as line with alpha=0.1",{
  ggiris <- iris.base + theme(panel.grid.major=element_line(linetype="dashed"))
  info <- gg2list(ggiris)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$gridcolor, toRGB("white", 0.1))
  }

  save_outputs(ggiris, "theme-dashed-grid-lines")
})

countrypop <- data.frame(country=c("Paraguay", "Peru", "Philippines"),
                         population=c(7, 31, 101),
                         edu=c(4.2, 1.75, 1.33),
                         illn=c(0.38, 1.67, 0.43))
gg <- ggplot(countrypop) +
  geom_point(aes(edu, illn, colour=country, size=population))

test_that("marker default shape is a circle", {
  info <- gg2list(gg)
  for (i in c(1:3)) {
    expect_identical(info[[i]]$marker$symbol, "circle")
  }
  
  save_outputs(gg, "theme-marker-default")
})

test_that("plot panel border is translated correctly", {
  ggiris <- iris.base + theme_grey() # has no panel.border
  info <- gg2list(ggiris)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$showline, FALSE)
  }

  save_outputs(ggiris, "theme-panel-border-1")

  red <- ggplot(iris) +
    theme_grey()+
    geom_point(aes(Petal.Width, Sepal.Width)) +
    theme(panel.border=element_rect(colour="red", fill=NA))
  info <- gg2list(red)
  for (xy in c("x", "y")) {
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$showline, TRUE)
    expect_identical(ax.list$linecolor, toRGB("red"))
  }

  save_outputs(red, "theme-panel-border-2")
})
