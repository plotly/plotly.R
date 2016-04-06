context("geom_point")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("smooth-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, layout=L$layout)
}

test_that("geom_point size & alpha translate to a single trace", {
  gg <- ggplot(mtcars, aes(cyl, wt)) + 
    geom_point(aes(size = gear, alpha = cyl)) 
  info <- save_outputs(gg, "point-size-alpha")
  expect_equal(length(info$data), 1)
  mkr <- info$data[[1]]$marker
  expect_equal(length(mkr$size), nrow(mtcars))
  expect_equal(length(mkr$opacity), nrow(mtcars))
})

test_that("marker color is non-transparent for open shapes", {
  p <- ggplot(mtcars, aes(mpg, wt)) + geom_point(pch = 2)
  info <- save_outputs(p, "open-shapes")
  expect_true(
    grepl("open$", info$data[[1]]$marker$symbol)
  )
  expect_true(
    info$data[[1]]$marker$color == toRGB(GeomPoint$default_aes$colour)
  )
})

test_that("marker color inherits from fill, when appropriate", {
  df_shapes <- data.frame(shape = factor(0:24))
  p <- ggplot(df_shapes, aes(shape = shape)) +
    geom_point(aes(shape = shape, x = 0, y = 0), size = 5, fill = "red") +
    facet_wrap(~shape) +
    scale_shape_manual(values = df_shapes$shape, guide = "none")
  l <- save_outputs(p, "all-shapes")
  expect_equal(length(l$data), 25)
  markerColors <- sapply(l$data, function(x) x$marker$color)
  lineColors <- sapply(l$data, function(x) x$marker$line$color)
  expect_true(all(markerColors[1:20] == lineColors[1:20]))
  expect_true(all(markerColors[21:25] != lineColors[21:25]))
})


test_that("can plot on sub-second time scale", {
  d <- data.frame(
    x = Sys.time() + 1e-3 * c(1:9, 5000), 
    y = rnorm(10)
  )
  g <- ggplot(d, aes(x, y)) + geom_point()
  info <- save_outputs(g, "point-size-alpha2")
  expect_equivalent(info$data[[1]]$x, as.numeric(d$x))
})


test_that("tickvals/ticktext are appropriately boxed", {
  d <- data.frame(x = factor(75), y = 10)
  p <- qplot(x, y, data = d) 
  info <- save_outputs(p, "point-box")
  expect_true(plotly:::to_JSON(info$layout$xaxis$tickvals) == '[1]')
  expect_true(plotly:::to_JSON(info$layout$xaxis$ticktext) == '["75"]')
})
