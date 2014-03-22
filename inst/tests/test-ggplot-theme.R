context("ggplot themes")

test_that("background translated correctly",{
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))+
    theme(panel.background=element_rect(fill="blue"))+
    theme(plot.background=element_rect(fill="green"))
  info <- gg2list(ggiris)
  L <- info$kwargs$layout
  expect_identical(L$plot_bgcolor, toRGB("blue"))
  expect_identical(L$paper_bgcolor, toRGB("green"))
})

test_that("grid/ticks translated correctly",{
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))+
    theme(axis.ticks=element_line(colour="red"))+
    theme(panel.grid.major=element_line(colour="violet"))
  info <- gg2list(ggiris)
  for(xy in c("x", "y")){
    ax.list <- info$kwargs$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$tickcolor, toRGB("red"))
    expect_identical(ax.list$gridcolor, toRGB("violet"))
  }
})


  
