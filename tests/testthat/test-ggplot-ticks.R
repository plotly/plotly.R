context("ggplot ticks")

boxes <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()

expect_traces <- function(gg, n.traces){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  L <- gg2list(gg)
  is.trace <- names(L) == ""
  traces <- L[is.trace]
  expect_equal(length(traces), n.traces)
  list(traces=traces, kwargs=L$kwargs)
}

plant.list <- split(PlantGrowth, PlantGrowth$group)

test_that("boxes without coord_flip()", {
  info <- expect_traces(boxes, 3)
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }
})

test_that("boxes with coord_flip()", {
  flipped <- boxes + coord_flip()
  info <- expect_traces(flipped, 3)
  for(tr in info$traces){
    expect_true(is.null(tr[["y"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["x"]]
    expect_equal(computed, expected)
  }
})


test_that("Manually set the order of a discrete-valued axis", {
  expected.order <- c("trt1", "ctrl", "trt2")
  boxes.limits <- boxes + scale_x_discrete(limits=expected.order)
  info <- expect_traces(boxes.limits, 3)
  computed.order <- sapply(info$traces, "[[", "name")
  expect_identical(as.character(computed.order), expected.order)
})

expected.labels <- c("Control", "Treat 1", "Treat 2")
boxes.labels <- boxes +
  scale_x_discrete(breaks=c("ctrl", "trt1", "trt2"),
                   labels=expected.labels)

test_that("Manually set the order of a discrete-valued axis", {
  info <- expect_traces(boxes.labels, 3)
  computed.labels <- sapply(info$traces, "[[", "name")
  expect_identical(as.character(computed.labels), expected.labels)
})

no.breaks <- boxes + scale_x_discrete(breaks=NULL)

test_that("hide x ticks, lines, and labels", {
  info <- expect_traces(no.breaks, 3)
  x <- info$kwargs$layout$xaxis
  expect_identical(x[["showticklabels"]], FALSE)
  ##expect_identical(x[["showline"]], FALSE) #irrelevant.
  expect_identical(x[["showgrid"]], FALSE)

  ## ticks ('' | 'inside' | 'outside') Sets the format of the ticks on
  ## this axis. For hidden ticks, link 'ticks' to an empty string.
  expect_identical(x[["ticks"]], "")

  ## xaxis has parameter autotick (a boolean: TRUE | FALSE) Toggle
  ## whether or not the axis ticks parameters are picked automatically
  ## by Plotly. Once 'autotick' is set to FALSE, the axis ticks
  ## parameters can be declared with 'ticks', 'tick0', 'dtick0' and
  ## other tick-related key in this axis object.
  ##expect_identical(x[["autotick"]], FALSE) #not necessary
})

test_that("Hide X ticks and labels, but keep the gridlines" {
  boxes.grid <- boxes +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  info <- expect_traces(boxes.grid, 3)
  x <- info$kwargs$layout$xaxis
  expect_identical(x[["showticklabels"]], FALSE)
  expect_identical(x[["showgrid"]], TRUE)
  expect_identical(x[["ticks"]], "")
})

test_that("Set continuous Y axis range", {
  boxes.range <- boxes + ylim(0,8)
  boxes.range <- boxes + scale_y_continuous(limits=c(0,8))
  info <- expect_traces(boxes.range, 3)
  ## TODO: can plotly zoom be specified?
})

test_that("Reverse order of a continuous-valued axis", {
  boxes.reverse <- boxes + scale_y_reverse()
  ##TODO
})

test_that("Set the X tick mark locations", {
  ## This will show tick marks on every 0.25 from 1 to 10. The scale will
  ## show only the ones that are within range (3.50-6.25 in this case)
  boxes.ticks <- boxes + scale_y_continuous(breaks=seq(1,10,1/4))
  ##TODO
})

test_that("The breaks can be spaced unevenly", {
  boxes.uneven <- boxes + scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6,8))
  ##TODO
})

test_that("Suppress ticks and gridlines", {
  ticks.nobreaks <- boxes + scale_y_continuous(breaks=NULL)
  ##TODO
})

test_that("Hide tick marks and labels (on Y axis), but keep the gridlines", {
  boxes.ygrid <- boxes +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  ##TODO
})
