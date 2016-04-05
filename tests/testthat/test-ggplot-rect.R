context("geom_rect")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("rect-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, layout=L$layout)
}

df <- data.frame(
  x = sample(10, 20, replace = TRUE),
  y = sample(10, 20, replace = TRUE)
)

gg <- ggplot(df, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 2)) +
  geom_rect()

test_that('geom_rect becomes 1 trace with mode="lines" fill="tozerox"', {
  info <- expect_traces(gg, 1, "black")
  tr <- info$traces[[1]]
  expect_identical(tr$fill, "tozerox")
  expect_identical(tr$type, "scatter")
  expect_identical(tr$mode, "lines")
  for(xy in c("x", "y")) {
    expect_true(anyNA(tr[[xy]]))
  }
})

df4 <- data.frame(
  x = 1:4, 
  status = c("cool", "not", "not", "cool")
)

gg4 <- ggplot(df4, aes(xmin = x, xmax = x + 0.5, ymin = 0, ymax = 1)) +
  geom_rect()

test_that('trace contains NA back to 1st rect', {
  info <- expect_traces(gg4, 1, "black4")
  tr <- info$traces[[1]]
  expect_identical(tr$fill, "tozerox")
  expect_identical(tr$type, "scatter")
  expect_identical(tr$mode, "lines")
  expected.x <- c(1, 1, 1.5, 1.5, 1, NA,
                  2, 2, 2.5, 2.5, 2, NA,
                  3, 3, 3.5, 3.5, 3, NA,
                  4, 4, 4.5, 4.5, 4, NA,
                  3, NA,
                  2, NA,
                  1, 1)
  expect_equal(tr$x, expected.x)
  expected.y <- c(0, 1, 1, 0, 0, NA,
                  0, 1, 1, 0, 0, NA,
                  0, 1, 1, 0, 0, NA,
                  0, 1, 1, 0, 0, NA,
                  0, NA,
                  0, NA,
                  0, 0)
  expect_equal(tr$y, expected.y)
})

rect.color <- ggplot(df4, aes(xmin = x, xmax = x + 0.5, ymin = 0, ymax = 1)) +
  geom_rect(aes(color = status), fill="grey")

test_that('rect color', {
  info <- expect_traces(rect.color, 2, "color")
  traces.by.name <- list()
  for(tr in info$traces){
    expect_true(tr$fillcolor == toRGB("grey"))
    expect_true(tr$fill == "tozerox")
    expect_equal(tr$y,
                 c(0, 1, 1, 0, 0, NA,
                   0, 1, 1, 0, 0, NA,
                   0, 0))
    traces.by.name[[tr$name]] <- tr
  }
  expect_equal(traces.by.name[[1]]$x,
               c(1, 1, 1.5, 1.5, 1, NA,
                 4, 4, 4.5, 4.5, 4, NA,
                 1, 1))
  expect_equal(traces.by.name[[2]]$x,
               c(2, 2, 2.5, 2.5, 2, NA,
                 3, 3, 3.5, 3.5, 3, NA,
                 2, 2))
  expect_false(traces.by.name[[1]]$line$color ==
               traces.by.name[[2]]$line$color)
})

rect.fill <- ggplot(df4, aes(xmin = x, xmax = x + 0.5, ymin = 0, ymax = 1)) +
  geom_rect(aes(fill = status))

test_that('rect color', {
  info <- expect_traces(rect.fill, 2, "fill")
  traces.by.name <- list()
  for(tr in info$traces){
    expect_true(tr$line$color == "transparent")
    expect_true(tr$fill == "tozerox")
    expect_equal(tr$y,
                 c(0, 1, 1, 0, 0, NA,
                   0, 1, 1, 0, 0, NA,
                   0, 0))
    traces.by.name[[tr$name]] <- tr
  }
  expect_equal(traces.by.name[[1]]$x,
               c(1, 1, 1.5, 1.5, 1, NA,
                 4, 4, 4.5, 4.5, 4, NA,
                 1, 1))
  expect_equal(traces.by.name[[2]]$x,
               c(2, 2, 2.5, 2.5, 2, NA,
                 3, 3, 3.5, 3.5, 3, NA,
                 2, 2))
  expect_false(traces.by.name[[1]]$fillcolor ==
               traces.by.name[[2]]$fillcolor)
})

rect.fill.color <-
  ggplot(df4, aes(xmin = x, xmax = x + 0.5, ymin = 0, ymax = 1)) +
  geom_rect(aes(fill = status), color="black")

test_that('rect aes(fill) with constant color', {
  info <- expect_traces(rect.fill.color, 2, "fill-color")
  traces.by.name <- list()
  for(tr in info$traces){
    expect_true(tr$line$color == toRGB("black"))
    expect_true(tr$fill == "tozerox")
    expect_equal(tr$y,
                 c(0, 1, 1, 0, 0, NA,
                   0, 1, 1, 0, 0, NA,
                   0, 0))
    traces.by.name[[tr$name]] <- tr
  }
  expect_equal(traces.by.name[[1]]$x,
               c(1, 1, 1.5, 1.5, 1, NA,
                 4, 4, 4.5, 4.5, 4, NA,
                 1, 1))
  expect_equal(traces.by.name[[2]]$x,
               c(2, 2, 2.5, 2.5, 2, NA,
                 3, 3, 3.5, 3.5, 3, NA,
                 2, 2))
  expect_false(traces.by.name[[1]]$fillcolor ==
               traces.by.name[[2]]$fillcolor)
})
