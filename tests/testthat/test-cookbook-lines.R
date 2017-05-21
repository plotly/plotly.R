context("cookbook lines")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("cookbook-axes-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

# Some sample data
df <- data.frame(
  cond = c("control", "treatment"),
  result  = c(10, 11.5),
  hline = c(9, 12)
)

# Basic bar plot
bp <- ggplot(df, aes(x = cond, y = result)) +
  geom_bar(position = "dodge", stat = "identity")

test_that("geom_bar -> 1 trace", {
  info <- expect_traces(bp, 1, "basic-bar")
})

# Add a horizontal line
temp <- bp + geom_hline(aes(yintercept = 12))
test_that("bar + hline = 2 traces", {
  info <- expect_traces(temp, 2, "basic-horizontal-line")
})

# Make the line red and dashed
temp <- bp + geom_hline(aes(yintercept=12), colour="#990000", linetype="dashed")
test_that("bar + red dashed hline", {
  info <- expect_traces(temp, 2, "dashed-red-line")
  hline.info <- info$data[[2]]
  expect_identical(hline.info$line$color, toRGB("#990000"))
  expect_identical(hline.info$line$dash, "dash")
})


# Need to re-specify bp, because the data has changed
bp <- ggplot(df, aes(x=cond, y=result)) +
  geom_bar(position=position_dodge(), stat="identity")

bp.err <- bp +
  geom_errorbar(aes(y = hline, ymax = hline, ymin = hline), 
                colour = "#AA0000")
test_that("Draw with separate lines for each bar", {
  expect_traces(bp.err, 2, "bar-error-wide")
})

bp.err.narrow <- bp +
  geom_errorbar(width = 0.5, aes(y = hline, ymax = hline, ymin = hline),
                colour = "#AA0000")
test_that("Make the lines narrower", {
  info <- expect_traces(bp.err.narrow, 2,  "bar-error-narrow")
})


# Can get the same result, even if we get the hline values from a second data frame
# Define data frame with hline
df.hlines <- data.frame(
  cond = c("control","treatment"), 
  hline = c(9,12)
)


bp.err.diff <- bp +
  geom_errorbar(data = df.hlines, aes(y = hline, ymax = hline, ymin = hline),
                colour = "#AA0000")
test_that("The bar graph are from df, but the lines are from df.hlines", {
  info <- expect_traces(bp.err.diff, 2,  "bar-error-diff")
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
  info <- expect_traces(bp, 2, "bar-dodge-color")
})

bp.err <- 
  bp + geom_errorbar(aes(y = hline, ymax = hline, ymin = hline), 
                     linetype = "dashed")

test_that("The error bars get plotted over one another", {
  info <- expect_traces(bp.err, 4, "bar-dodge-color-error")
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
  geom_errorbar(aes(y = hline, ymax = hline + 1, ymin = hline - 1),
                linetype = "dashed", position = position_dodge())

test_that("4 error bars", {
  info <- expect_traces(bp.err4, 4, "bar-dodge-color-err4")
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
  info <- expect_traces(sp, 2, "scatter-basic")
})

temp <- sp + geom_hline(aes(yintercept=10))
test_that("Add a horizontal line", {
  info <- expect_traces(temp, 3, "scatter-hline")
})

temp <- sp +
  geom_hline(aes(yintercept = 10)) +
  geom_vline(aes(xintercept = 11.5),
             colour = "#BB0000", linetype = "dashed")
test_that("Add a red dashed vertical line", {
  info <- expect_traces(temp, 4, "scatter-hline-vline")
  expect_true(info$layout$showlegend)
  mode <- sapply(info$data, "[[", "mode")
  line.traces <- info$data[mode == "lines"]
  expect_equivalent(length(line.traces), 2)
  dash <- sapply(line.traces, function(tr)tr$line$dash)
  dash.traces <- line.traces[dash == "dash"]
  expect_equivalent(length(dash.traces), 1)
  dash.trace <- dash.traces[[1]]
  expect_identical(dash.trace$line$color, toRGB("#BB0000"))
})

# Facet, based on cond
spf <- sp + facet_grid(. ~ cond)
test_that("scatter facet -> 2 traces", {
  info <- expect_traces(spf, 2, "scatter-facet")
  expect_true(info$data[[1]]$xaxis != info$data[[2]]$xaxis)
  expect_true(info$data[[1]]$yaxis == info$data[[2]]$yaxis)
  # only one yaxis
  expect_equivalent(sum(grepl("yaxis", names(info$layout))), 1)
})

temp <- spf + geom_hline(aes(yintercept=10))
test_that("geom_hline -> 2 more traces", {
  info <- expect_traces(temp, 4, "scatter-facet-hline")
  
  expect_true(info$layout$showlegend)
  has.name <- sapply(info$data, function(tr) isTRUE(nchar(tr$name) > 0))
  expect_equivalent(sum(has.name), 2)
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
  info <- expect_traces(spf.vline, 6, "scatter-facet-hline-vline")
})
