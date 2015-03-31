context("cookbook lines")

expect_traces_shapes <- function(gg, n.traces, n.shapes, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  stopifnot(is.numeric(n.shapes))
  save_outputs(gg, paste0("cookbook-lines-", name))
  L <- gg2list(gg)
  is.trace <- names(L) == ""
  all.traces <- L[is.trace]
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  shapes <- L$kwargs$layout$shapes
  expect_equal(length(shapes), n.shapes)
  list(traces=has.data,
       shapes=shapes,
       kwargs=L$kwargs)
}

expect_shape <- function(s, ...){
  expected.list <- list(...)
  for(key in names(expected.list)){
    value <- expected.list[[key]]
    expect_identical(s[[key]], value)
  }
}

# Some sample data
df <- read.table(header=T, text="
     cond result
  control     10
treatment   11.5
")

# Basic bar plot
bp <- ggplot(df, aes(x=cond, y=result)) +
  geom_bar(position="dodge", stat="identity")

## info <- gg2list(bp)
## info$kwargs$layout$shapes <-
##   list(list(xref="paper",
##        x0=0,
##        x1=1,
##        yref="y1",
##        y0=10,
##        y1=10))
## sendJSON(info)

test_that("geom_bar -> 1 trace", {
  info <- expect_traces_shapes(bp, 1, 0, "basic-bar")
})

# Add a horizontal line
temp <- bp + geom_hline(aes(yintercept=12))
test_that("bar + hline = 1 trace, 1 shape", {
  info <- expect_traces_shapes(temp, 1, 1, "basic-horizontal-line")
  expect_shape(info$shapes[[1]],
               xref="paper", x0=0, x1=1,
               yref="y1", y0=12, y1=12)
})

# Make the line red and dashed
temp <- bp + geom_hline(aes(yintercept=12), colour="#990000", linetype="dashed")
test_that("bar + red dashed hline", {
  info <- expect_traces(temp, 2, "dashed-red-line")
  info$traces[[2]]
})

# Draw separate hlines for each bar. First add another column to df
df$hline <- c(9,12)
#      cond result hline
#   control   10.0     9
# treatment   11.5    12

# Need to re-specify bp, because the data has changed
bp <- ggplot(df, aes(x=cond, y=result)) + geom_bar(position=position_dodge())

# Draw with separate lines for each bar
bp + geom_errorbar(aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")

# Make the lines narrower
bp + geom_errorbar(width=0.5, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")


# Can get the same result, even if we get the hline values from a second data frame
# Define data frame with hline
df.hlines <- data.frame(cond=c("control","treatment"), hline=c(9,12))
#      cond hline
#   control     9
# treatment    12

# The bar graph are from df, but the lines are from df.hlines
bp + geom_errorbar(data=df.hlines, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")

df <- read.table(header=T, text="
     cond group result hline
  control     A     10     9
treatment     A   11.5    12
  control     B     12     9
treatment     B     14    12
")

# Define basic bar plot
bp <- ggplot(df, aes(x=cond, y=result, fill=group)) + geom_bar(position=position_dodge())
bp

# The error bars get plotted over one another -- there are four but it looks like two
bp + geom_errorbar(aes(y=hline, ymax=hline, ymin=hline), linetype="dashed")

df <- read.table(header=T, text="
     cond group result hline
  control     A     10    11
treatment     A   11.5    12
  control     B     12  12.5
treatment     B     14    15
")

# Define basic bar plot
bp <- ggplot(df, aes(x=cond, y=result, fill=group)) + geom_bar(position=position_dodge())
bp

bp + geom_errorbar(aes(y=hline, ymax=hline, ymin=hline), linetype="dashed", position=position_dodge())

df <- read.table(header=T, text="
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

library(ggplot2)

# The basic scatterplot
sp <- ggplot(df, aes(x=xval, y=yval, colour=cond)) + geom_point()


# Add a horizontal line
temp <- sp + geom_hline(aes(yintercept=10))
save_outputs(temp, "lines/hline on scatter", file_prefix="")

# Add a red dashed vertical line
temp <- sp + geom_hline(aes(yintercept=10)) + geom_vline(aes(xintercept=11.5), colour="#BB0000", linetype="dashed")
save_outputs(temp, "lines/hline n vline on scatter", file_prefix="")

# Add colored lines for the mean xval of each group
temp <- sp + geom_hline(aes(yintercept=10)) +
     geom_line(stat="vline", xintercept="mean")
save_outputs(temp, "lines/colored lines on scatter", file_prefix="")

# Facet, based on cond
spf <- sp + facet_grid(. ~ cond)
spf

# Draw a horizontal line in all of the facets at the same value
temp <- spf + geom_hline(aes(yintercept=10))
save_outputs(temp, "lines/hline on facets", file_prefix="")

df.vlines <- data.frame(cond=levels(df$cond), xval=c(10,11.5))
#      cond xval
#   control 10.0
# treatment 11.5

spf + geom_hline(aes(yintercept=10)) +
      geom_vline(aes(xintercept=xval), data=df.vlines,
                    colour="#990000", linetype="dashed")

spf + geom_hline(aes(yintercept=10)) +
      geom_line(stat="vline", xintercept="mean")
