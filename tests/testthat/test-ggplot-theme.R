context("ggplot themes")

iris.base <- ggplot(iris) +
  geom_point(aes(Petal.Width, Sepal.Width)) +
  theme_grey()

test_that("background translated correctly",{
  ggiris <- iris.base + 
    theme(panel.background = element_rect(fill = "blue"),
          plot.background = element_rect(fill = "green"))
  info <- save_outputs(ggiris, "theme-background")
  L <- info$layout
  expect_true(L$plot_bgcolor == toRGB("blue"))
  expect_true(L$paper_bgcolor == toRGB("green"))
})

test_that("grid/ticks translated correctly",{
  ggiris <- iris.base + 
    theme(axis.ticks = element_line(colour = "red"),
          panel.grid.major = element_line(colour = "violet")) 
  info <- save_outputs(ggiris, "theme-ticks-and-grids")
  for (xy in c("x", "y")) {
    ax.list <- info$layout[[paste0(xy, "axis")]]
    expect_true(ax.list$tickcolor == toRGB("red"))
    expect_true(ax.list$gridcolor == toRGB("violet"))
  }
})

test_that("show ticks as 'outside' by default", {
  ggiris <- iris.base
  info <- save_outputs(ggiris, "theme-ticks-default")
  for (xy in c("x", "y")) {
    ax.list <- info$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$ticks, "outside")
  }
})

test_that("do not show zeroline by default", {
  ggiris <- iris.base
  info <- save_outputs(ggiris, "theme-zeroline-default")
  for (xy in c("x", "y")) {
    ax.list <- info$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$zeroline, FALSE)
  }
})

countrypop <- data.frame(
  country = c("Paraguay", "Peru", "Philippines"),
  population = c(7, 31, 101),
  edu = c(4.2, 1.75, 1.33),
  illn = c(0.38, 1.67, 0.43)
)

gg <- ggplot(countrypop) +
  geom_point(aes(edu, illn, colour = country, size = population))

test_that("marker default shape is a circle", {
  info <- save_outputs(gg, "theme-marker-default")
  for (i in c(1:3)) {
    expect_equivalent(info$data[[i]]$marker$symbol, "circle")
    expect_true(info$data[[i]]$showlegend)
  }
})

test_that("plot panel border is translated correctly", {
  ggiris <- iris.base + theme_grey() # has no panel.border
  info <- save_outputs(ggiris, "theme-panel-border-1")
  
  red <- ggplot(iris) +
    theme_grey() +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    theme(panel.border = element_rect(colour = "red", fill = NA))
  
  info <- save_outputs(red, "theme-panel-border-2")
  expect_true(info$layout$shapes[[1]]$line$color == toRGB("red"))
})
