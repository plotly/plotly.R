context("polygon")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("polygon-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

poly.df <- data.frame(
  x = c(0, 1, 1, 0, 2, 3, 3, 2) + 10,
  y = c(0, 0, 1, 1, 0, 0, 1, 1),
  g = c(1, 1, 1, 1, 2, 2, 2, 2),
  lab = rep(c("left", "right"), each = 4)
)

test_that("polygons with different hovertext must be different traces ", {
  gg <- ggplot(poly.df) + geom_polygon(aes(x, y, group = lab))
  info <- expect_traces(gg, 2, "black")
  expect_equivalent(info$data[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equivalent(info$data[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equivalent(info$data[[1]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(info$data[[2]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(unique(sapply(info$data, "[[", "fill")), "toself")
  expect_equivalent(unique(sapply(info$data, "[[", "hoveron")), "fills")
  expect_equivalent(sapply(info$data, "[[", "text"), c("lab: left", "lab: right"))
})

test_that("polygons with identical fill and hovertext generate one trace", {
  gg <- ggplot(poly.df) + geom_polygon(aes(x, y, group = lab))
  info <- plotly_build(ggplotly(gg, tooltip = NULL))$x
  expect_equivalent(length(info$data), 1)
  expect_equivalent(info$data[[1]]$x, c(10, 11, 11, 10, 10, NA, 12, 13, 13, 12, 12))
  expect_equivalent(info$data[[1]]$y, c(0, 0, 1, 1, 0, NA, 0, 0, 1, 1, 0))
  expect_equivalent(info$data[[1]]$fill, "toself")
  expect_equivalent(info$data[[1]]$hoveron, "fills")
  expect_equivalent(nchar(info$data[[1]]$text), 0)
})

blue.color <- rgb(0.23, 0.45, 0.67)

test_that("polygons with different color become separate traces", {
  gg <- ggplot(poly.df) +
    geom_polygon(aes(x, y, color = lab), fill = "grey") +
    scale_color_manual(values = c(left = blue.color, right = "springgreen3"))
  info <- expect_traces(gg, 2, "aes-color")
  traces.by.name <- list()
  for(tr in info$data){
    expect_equivalent(tr$fillcolor, toRGB("grey"))
    expect_equivalent(tr$fill, "toself")
    traces.by.name[[tr$name]] <- tr
  }
  expect_equivalent(traces.by.name[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equivalent(traces.by.name[[1]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(traces.by.name[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equivalent(traces.by.name[[2]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(traces.by.name[[1]]$line$color, toRGB(blue.color))
  expect_equivalent(traces.by.name[[2]]$line$color, toRGB("springgreen3"))
})

test_that("geom_polygon(aes(fill)) -> fillcolor + line$color transparent", {
  gg <- ggplot(poly.df) +
    geom_polygon(aes(x, y, fill = lab)) +
    scale_fill_manual(values = c(left = blue.color, right = "springgreen3"))
  info <- expect_traces(gg, 2, "aes-fill")
  traces.by.name <- list()
  for(tr in info$data){
    expect_true(tr$line$color == "transparent")
    traces.by.name[[tr$name]] <- tr
  }
  expect_equivalent(traces.by.name[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equivalent(traces.by.name[[1]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(traces.by.name[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equivalent(traces.by.name[[2]]$y, c(0, 0, 1, 1, 0))
  expect_true(traces.by.name[[1]]$fillcolor == toRGB(blue.color))
  expect_true(traces.by.name[[2]]$fillcolor == toRGB("springgreen3"))
})

test_that("geom_polygon(aes(fill), color) -> line$color", {
  gg <- ggplot(poly.df) +
    geom_polygon(aes(x, y, fill = lab), color = "black")+
    scale_fill_manual(values = c(left = blue.color, right = "springgreen3"))
  info <- expect_traces(gg, 2, "color-aes-fill")
  traces.by.name <- list()
  for(tr in info$data){
    expect_true(tr$line$color == toRGB("black"))
    expect_true(tr$fill == "toself")
    traces.by.name[[tr$name]] <- tr
  }
  expect_equivalent(traces.by.name[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equivalent(traces.by.name[[1]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(traces.by.name[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equivalent(traces.by.name[[2]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(traces.by.name[[1]]$fillcolor, toRGB(blue.color))
  expect_equivalent(traces.by.name[[2]]$fillcolor, toRGB("springgreen3"))
})

test_that("geom_polygon(aes(linetype), fill, color)", {
  gg <- ggplot(poly.df) +
    geom_polygon(aes(x, y, linetype = lab), fill = "red", colour = "blue")+
    scale_linetype_manual(values = c(left = "dotted", right = "dashed"))
  info <- expect_traces(gg, 2, "color-fill-aes-linetype")
  traces.by.name <- list()
  for(tr in info$data){
    expect_true(tr$fillcolor == toRGB("red"))
    expect_true(tr$line$color == toRGB("blue"))
    expect_true(tr$fill == "toself")
    traces.by.name[[tr$name]] <- tr
  }
  expect_equivalent(traces.by.name[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equivalent(traces.by.name[[1]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(traces.by.name[[1]]$line$dash, "dot")
  expect_equivalent(traces.by.name[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equivalent(traces.by.name[[1]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(traces.by.name[[2]]$line$dash, "dash")
})

test_that("geom_polygon(aes(size), fill, colour)", {
  gg <- ggplot(poly.df) +
    geom_polygon(aes(x, y, size = lab), fill = "orange", colour = "black") +
    scale_size_manual(values = c(left = 2, right = 3))
  info <- expect_traces(gg, 2, "color-fill-aes-size")
  traces.by.name <- list()
  for(tr in info$data){
    expect_true(tr$fillcolor == toRGB("orange"))
    expect_true(tr$line$color == toRGB("black"))
    expect_true(tr$fill == "toself")
    traces.by.name[[tr$name]] <- tr
  }
  expect_equivalent(traces.by.name[[1]]$x, c(10, 11, 11, 10, 10))
  expect_equivalent(traces.by.name[[1]]$y, c(0, 0, 1, 1, 0))
  expect_equivalent(traces.by.name[[2]]$x, c(12, 13, 13, 12, 12))
  expect_equivalent(traces.by.name[[2]]$y, c(0, 0, 1, 1, 0))
  expect_false(traces.by.name[[1]]$line$width ==
               traces.by.name[[2]]$line$width)
})

test_that("borders become one trace with NA", {
  gg <- ggplot(maps::canada.cities, aes(long, lat)) +
    borders(regions = "canada")
  info <- save_outputs(gg, "polygons-canada-borders")
  expect_equivalent(length(info$data), 1)
  expect_true(any(is.na(info$data[[1]]$x)))
  expect_equivalent(nchar(info$data[[1]]$text), 0)
})

x <- c(0, -1, 2, -2, 1)
y <- c(2, 0, 1, 1, 0)
stars <-rbind(
  data.frame(x, y, group = "left"),
  data.frame(x = x + 10, y, group = "right")
)
star.group <- ggplot(stars) +
  geom_polygon(aes(x, y, group = group))

test_that("geom_polygon(aes(group)) -> 1 trace", {
  info <- expect_traces(star.group, 1, "star-group")
  tr <- info$data[[1]]
  expect_equivalent(tr$fill, "toself")
  expect_equivalent(
    tr$x, c(0, -1, 2, -2, 1, 0, NA, 10, 9, 12, 8, 11, 10)
  )
  expect_equivalent(
    tr$y, c(2, 0, 1, 1, 0, 2, NA, 2, 0, 1, 1, 0, 2)
  )
})

star.group.color <- ggplot(stars) +
  geom_polygon(aes(x, y, group = group), color = "red")

test_that("geom_polygon(aes(group), color) -> 1 trace", {
  info <- expect_traces(star.group.color, 1, "star-group-color")
  tr <- info$data[[1]]
  expect_true(tr$fill == "toself")
  expect_true(tr$line$color == toRGB("red"))
  expect_equivalent(
    tr$x, c(0, -1, 2, -2, 1, 0, NA, 10, 9, 12, 8, 11, 10)
  )
  expect_equivalent(
    tr$y, c(2, 0, 1, 1, 0, 2, NA, 2, 0, 1, 1, 0, 2)
  )
})

star.fill.color <- ggplot(stars) +
  geom_polygon(aes(x, y, group = group, fill = group), color = "black")

test_that("geom_polygon(aes(group, fill), color) -> 2 trace", {
  info <- expect_traces(star.fill.color, 2, "star-fill-color")
  tr <- info$data[[1]]
  traces.by.name <- list()
  for(tr in info$data){
    expect_true(tr$line$color == toRGB("black"))
    expect_true(tr$fill == "toself")
    expect_equivalent(tr$y, c(2, 0, 1, 1, 0, 2))
    traces.by.name[[tr$name]] <- tr
  }
  expect_equivalent(traces.by.name[[1]]$x, c(0, -1, 2, -2, 1, 0))
  expect_equivalent(traces.by.name[[2]]$x, c(10, 9, 12, 8, 11, 10))
})
