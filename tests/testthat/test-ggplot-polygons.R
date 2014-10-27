context("polygon")

test_that("filled polygons become several traces", {
  poly.df <- data.frame(x=c(0, 1, 1, 0, 2, 3, 3, 2)+10,
                        y=c(0, 0, 1, 1, 0, 0, 1, 1)+10,
                        g=c(1, 1, 1, 1, 2, 2, 2, 2))
  poly.df$lab <- paste0("name", poly.df$g)
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, group=g))
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  expect_equal(info[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equal(info[[1]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equal(info[[2]]$y, c(10, 10, 11, 11, 10))

  save_outputs(gg, "polygons-filled-polygons")

  first.color <- rgb(0.23, 0.45, 0.67)
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, color=lab), fill="grey")+
    scale_color_manual(values=c(name1=first.color, name2="springgreen3"))
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  expect_equal(info[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equal(info[[1]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[1]]$fillcolor, toRGB("grey"))
  expect_equal(info[[1]]$line$color, toRGB(first.color))
  expect_equal(info[[1]]$name, "name1")
  expect_equal(info[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equal(info[[2]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[2]]$fillcolor, toRGB("grey"))
  expect_equal(info[[2]]$line$color, toRGB("springgreen3"))
  expect_equal(info[[2]]$name, "name2")

  save_outputs(gg, "polygons-springgreen3")


  first.color <- rgb(0.23, 0.45, 0.67)
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, fill=lab))+
    scale_fill_manual(values=c(name1=first.color, name2="springgreen3"))
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  expect_equal(info[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equal(info[[1]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[1]]$fillcolor, toRGB(first.color))
  expect_equal(info[[1]]$name, "name1")
  expect_equal(info[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equal(info[[2]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[2]]$fillcolor, toRGB("springgreen3"))
  expect_equal(info[[2]]$name, "name2")

  save_outputs(gg, "polygons-springgreen3-lab")


  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, linetype=lab), fill="red", colour="blue")+
    scale_linetype_manual(values=c(name1="dotted", name2="dashed"))
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  expect_equal(info[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equal(info[[1]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[1]]$fillcolor, toRGB("red"))
  expect_equal(info[[1]]$line$color, toRGB("blue"))
  expect_equal(info[[1]]$line$dash, "dot")
  expect_equal(info[[1]]$name, "name1")
  expect_equal(info[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equal(info[[2]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[2]]$fillcolor, toRGB("red"))
  expect_equal(info[[2]]$line$color, toRGB("blue"))
  expect_equal(info[[2]]$line$dash, "dash")
  expect_equal(info[[2]]$name, "name2")

  save_outputs(gg, "polygons-dashed")


  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, size=lab), fill="orange", colour="black")+
    scale_size_manual(values=c(name1=2, name2=3))
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  expect_equal(info[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equal(info[[1]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[1]]$fillcolor, toRGB("orange"))
  expect_equal(info[[1]]$line$width, 2)
  expect_equal(info[[1]]$name, "name1")
  expect_equal(info[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equal(info[[2]]$y, c(10, 10, 11, 11, 10))
  expect_equal(info[[2]]$fillcolor, toRGB("orange"))
  expect_equal(info[[2]]$line$width, 3)
  expect_equal(info[[2]]$name, "name2")


  save_outputs(gg, "polygons-halloween")

})

test_that("borders become one trace with NA", {
  library(maps)
  data(canada.cities)
  gg <- ggplot(canada.cities, aes(long, lat))+
    borders(regions="canada", name="borders")
  info <- gg2list(gg)
  expect_identical(length(info), 2L)
  tr <- info[[1]]
  expect_true(any(is.na(tr$x)))

  save_outputs(gg, "polygons-borders")
})
