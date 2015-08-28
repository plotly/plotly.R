context("cookbook lines")

expect_traces_shapes <- function(gg, n.traces, n.shapes, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  stopifnot(is.numeric(n.shapes))
  L <- save_outputs(gg, paste0("cookbook-lines-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  shapes <- L$layout$shapes
  expect_equal(length(shapes), n.shapes)
  list(traces = has.data, shapes = shapes, layout = L$layout)
}

expect_shape <- function(s, ...) {
  expected.list <- list(...)
  for(key in names(expected.list)) {
    value <- expected.list[[key]]
    expect_identical(s[[key]], value)
  }
}

# Some sample data
df <- read.table(header = T, text = "
     cond result
  control     10
treatment   11.5
")

# Basic bar plot
bp <- ggplot(df, aes(x = cond, y = result)) +
  geom_bar(position = "dodge", stat = "identity")

test_that("geom_bar -> 1 trace", {
  info <- expect_traces_shapes(bp, 1, 0, "basic-bar")
})

# Add a horizontal line
temp <- bp + geom_hline(aes(yintercept = 12))
test_that("bar + hline = 2 traces", {
  info <- expect_traces_shapes(temp, 2, 0, "basic-horizontal-line")
})

# Make the line red and dashed
temp <- bp + geom_hline(aes(yintercept=12), colour="#990000", linetype="dashed")
test_that("bar + red dashed hline", {
  info <- expect_traces_shapes(temp, 2, 0, "dashed-red-line")
  hline.info <- info$traces[[2]]
  expect_identical(hline.info$line$color, toRGB("#990000"))
  expect_identical(hline.info$line$dash, "dash")
})

# Draw separate hlines for each bar. First add another column to df
df$hline <- c(9,12)
#      cond result hline
#   control   10.0     9
# treatment   11.5    12

# Need to re-specify bp, because the data has changed
bp <- ggplot(df, aes(x=cond, y=result)) +
  geom_bar(position=position_dodge(), stat="identity")

bp.err <- bp +
  geom_errorbar(aes(y = hline, ymax = hline, ymin = hline), 
                colour = "#AA0000")
test_that("Draw with separate lines for each bar", {
  expect_traces_shapes(bp.err, 2, 0, "bar-error-wide")
})

bp.err.narrow <- bp +
  geom_errorbar(width = 0.5, aes(y = hline, ymax = hline, ymin = hline),
                colour = "#AA0000")
test_that("Make the lines narrower", {
  info <- expect_traces_shapes(bp.err.narrow, 2, 0, "bar-error-narrow")
})


# Can get the same result, even if we get the hline values from a second data frame
# Define data frame with hline
df.hlines <- data.frame(cond=c("control","treatment"), hline=c(9,12))
#      cond hline
#   control     9
# treatment    12

bp.err.diff <- bp +
  geom_errorbar(data = df.hlines, aes(y = hline, ymax = hline, ymin = hline),
                colour = "#AA0000")
test_that("The bar graph are from df, but the lines are from df.hlines", {
  info <- expect_traces_shapes(bp.err.diff, 2, 0, "bar-error-diff")
})

df <- read.table(header=T, text="
     cond group result hline
  control     A     10     9
treatment     A   11.5    12
  control     B     12     9
treatment     B     14    12
")
bp <- ggplot(df, aes(x = cond, y = result, fill = group)) +
  geom_bar(position = position_dodge(), stat = "identity")
test_that("bar dodged colored -> 1 trace", {
  info <- expect_traces_shapes(bp, 2, 0, "bar-dodge-color")
})
bp.err <- 
  bp + geom_errorbar(aes(y = hline, ymax = hline, ymin = hline), 
                     linetype = "dashed")
test_that("The error bars get plotted over one another", {
  # there are four but it looks like two.
  info <- expect_traces_shapes(bp.err, 3, 0, "bar-dodge-color-error")
  err.y <- info$traces[[3]]$y
  expect_equal(length(err.y), 4)
  expect_equal(length(unique(err.y)), 2)
})

df <- read.table(header = TRUE, text = "
     cond group result hline
  control     A     10    11
treatment     A   11.5    12
  control     B     12  12.5
treatment     B     14    15
")
bp <- ggplot(df, aes(x = cond, y = result, fill = group)) +
  geom_bar(position = position_dodge(), stat = "identity")
bp.err4 <- bp +
  geom_errorbar(aes(y = hline, ymax = hline, ymin = hline),
                linetype = "dashed", position = position_dodge())
test_that("4 error bars", {
  info <- expect_traces_shapes(bp.err4, 3, 0, "bar-dodge-color-err4")
  tr <- info$traces[[3]]
  expect_equal(length(tr$y), 4)
  expect_equal(length(unique(tr$y)), 4)
  expect_equal(length(tr$x), 4)
  expect_equal(length(unique(tr$x)), 2)
})

df <- read.table(header = T, text = "
      cond xval yval
   control 11.5 10.8
   control  9.3 12.9
   control  8.0  9.9
   control 11.5 10.1
   control  8.6  8.3
   control  9.9  9.5
   control  8.8  8.7
   control 11.7 10.1
   control  9.7  9.3
   control  9.8 12.0
 treatment 10.4 10.6
 treatment 12.1  8.6
 treatment 11.2 11.0
 treatment 10.0  8.8
 treatment 12.9  9.5
 treatment  9.1 10.0
 treatment 13.4  9.6
 treatment 11.6  9.8
 treatment 11.5  9.8
 treatment 12.0 10.6
")
sp <- ggplot(df, aes(x = xval, y = yval, colour = cond)) + geom_point()
test_that("basic scatterplot", {
  info <- expect_traces_shapes(sp, 2, 0, "scatter-basic")
})

temp <- sp + geom_hline(aes(yintercept=10))
test_that("Add a horizontal line", {
  info <- expect_traces_shapes(temp, 3, 0, "scatter-hline")
})

temp <- sp +
  geom_hline(aes(yintercept = 10)) +
  geom_vline(aes(xintercept = 11.5),
             colour = "#BB0000", linetype = "dashed")
test_that("Add a red dashed vertical line", {
  info <- expect_traces_shapes(temp, 4, 0, "scatter-hline-vline")
  expect_true(info$layout$showlegend)
  mode <- sapply(info$traces, "[[", "mode")
  line.traces <- info$traces[mode == "lines"]
  expect_equal(length(line.traces), 2)
  dash <- sapply(line.traces, function(tr)tr$line$dash)
  dash.traces <- line.traces[dash == "dash"]
  expect_equal(length(dash.traces), 1)
  dash.trace <- dash.traces[[1]]
  expect_identical(dash.trace$line$color, toRGB("#BB0000"))
})

# Facet, based on cond
spf <- sp + facet_grid(. ~ cond)
test_that("scatter facet -> 2 traces", {
  info <- expect_traces_shapes(spf, 2, 0, "scatter-facet")
  expect_true(info$traces[[1]]$xaxis != info$traces[[2]]$xaxis)
  expect_true(info$traces[[1]]$yaxis == info$traces[[2]]$yaxis)
})

temp <- spf + geom_hline(aes(yintercept=10))
test_that("geom_hline -> 2 more traces", {
  info <- expect_traces_shapes(temp, 4, 0, "scatter-facet-hline")
  expect_true(info$layout$showlegend)
  has.name <- sapply(info$traces, function(tr)is.character(tr$name))
  named.traces <- info$traces[has.name]
  expect_equal(length(named.traces), 2)
})

df.vlines <- data.frame(cond = levels(df$cond), xval = c(10,11.5))
#      cond xval
#   control 10.0
# treatment 11.5

spf.vline <- 
  spf +
  geom_hline(aes(yintercept = 10)) +
  geom_vline(aes(xintercept = xval),
             data = df.vlines,
             colour = "#990000", linetype = "dashed")
test_that("geom_vline -> 2 more traces", {
  info <- expect_traces_shapes(spf.vline, 6, 0, "scatter-facet-hline-vline")
})
