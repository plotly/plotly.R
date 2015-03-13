context("polygon")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("polygon-", name))
  L <- gg2list(gg)
  is.trace <- names(L) == ""
  all.traces <- L[is.trace]
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, kwargs=L$kwargs)
}

poly.df <- data.frame(x=c(0, 1, 1, 0, 2, 3, 3, 2)+10,
                      y=c(0, 0, 1, 1, 0, 0, 1, 1),
                      g=c(1, 1, 1, 1, 2, 2, 2, 2),
                      lab=rep(c("left", "right"), each=4))

test_that("polygons filled with the same color become one trace", {
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, group=g))
  info <- expect_traces(gg, 1, "black")
  tr <- info$traces[[1]]
  expected.x <-
    c(10, 11, 11, 10, 10, NA,
      12, 13, 13, 12, 12, NA,
      10, 10)
  expect_equal(tr$x, expected.x)
  expected.y <- 
    c(0, 0, 1, 1, 0, NA,
      0, 0, 1, 1, 0, NA,
      0, 0)
  expect_equal(tr$y, expected.y)
  expect_identical(tr$line$color, "transparent")
  expect_identical(tr$line$color, "transparent")
})

blue.color <- rgb(0.23, 0.45, 0.67)

test_that("polygons with different color become separate traces", {
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, color=lab), fill="grey")+
    scale_color_manual(values=c(left=blue.color, right="springgreen3"))
  info <- expect_traces(gg, 2, "aes-color")
  traces.by.name <- list()
  for(tr in info$traces){
    expect_equal(tr$fillcolor, toRGB("grey"))
    traces.by.name[[tr$name]] <- tr
  }
  expect_equal(traces.by.name$left$x, c(10, 11, 11, 10, 10))
  expect_equal(traces.by.name$left$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$right$x, c(12, 13, 13, 12, 12))
  expect_equal(traces.by.name$right$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$left$line$color, toRGB(blue.color))
  expect_equal(traces.by.name$right$line$color, toRGB("springgreen3"))
})

test_that("geom_polygon(aes(fill)) -> fillcolor + line$color transparent", {
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, fill=lab))+
    scale_fill_manual(values=c(left=blue.color, right="springgreen3"))
  info <- expect_traces(gg, 2, "aes-fill")
  traces.by.name <- list()
  for(tr in info$traces){
    expect_equal(tr$line$color, "transparent")
    traces.by.name[[tr$name]] <- tr
  }
  expect_equal(traces.by.name$left$x, c(10, 11, 11, 10, 10))
  expect_equal(traces.by.name$left$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$right$x, c(12, 13, 13, 12, 12))
  expect_equal(traces.by.name$right$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$left$fillcolor, toRGB(blue.color))
  expect_equal(traces.by.name$right$fillcolor, toRGB("springgreen3"))
})

test_that("geom_polygon(aes(fill), color) -> line$color", {
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, fill=lab), color="black")+
    scale_fill_manual(values=c(left=blue.color, right="springgreen3"))
  info <- expect_traces(gg, 2, "color-aes-fill")
  traces.by.name <- list()
  for(tr in info$traces){
    expect_equal(tr$line$color, toRGB("black"))
    traces.by.name[[tr$name]] <- tr
  }
  expect_equal(traces.by.name$left$x, c(10, 11, 11, 10, 10))
  expect_equal(traces.by.name$left$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$right$x, c(12, 13, 13, 12, 12))
  expect_equal(traces.by.name$right$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$left$fillcolor, toRGB(blue.color))
  expect_equal(traces.by.name$right$fillcolor, toRGB("springgreen3"))
})

test_that("geom_polygon(aes(linetype), fill, color)", {
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, linetype=lab), fill="red", colour="blue")+
    scale_linetype_manual(values=c(left="dotted", right="dashed"))
  info <- expect_traces(gg, 2, "color-fill-aes-linetype")
  traces.by.name <- list()
  for(tr in info$traces){
    expect_equal(tr$fillcolor, toRGB("red"))
    expect_equal(tr$line$color, toRGB("blue"))
    traces.by.name[[tr$name]] <- tr
  }
  expect_equal(traces.by.name$left$x, c(10, 11, 11, 10, 10))
  expect_equal(traces.by.name$left$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$left$line$dash, "dot")
  expect_equal(traces.by.name$right$x, c(12, 13, 13, 12, 12))
  expect_equal(traces.by.name$right$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$right$line$dash, "dash")
})

test_that("geom_polygon(aes(size), fill, colour)", {
  gg <- ggplot(poly.df)+
    geom_polygon(aes(x, y, size=lab), fill="orange", colour="black")+
    scale_size_manual(values=c(left=2, right=3))
  info <- expect_traces(gg, 2, "color-fill-aes-linetype")
  traces.by.name <- list()
  for(tr in info$traces){
    expect_equal(tr$fillcolor, toRGB("orange"))
    expect_equal(tr$line$color, toRGB("black"))
    traces.by.name[[tr$name]] <- tr
  }
  expect_equal(traces.by.name$left$x, c(10, 11, 11, 10, 10))
  expect_equal(traces.by.name$left$y, c(0, 0, 1, 1, 0))
  expect_equal(traces.by.name$right$x, c(12, 13, 13, 12, 12))
  expect_equal(traces.by.name$right$y, c(0, 0, 1, 1, 0))
  expect_false(traces.by.name$left$line$width ==
               traces.by.name$right$line$width)
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

  save_outputs(gg, "polygons-canada-borders")
})
