context("ggplot ticks")

PlantGrowth$type <-
  ifelse(PlantGrowth$group=="ctrl", "control", "treatment")
boxes <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("ticks-", name))
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

plant.list <- split(PlantGrowth, PlantGrowth$group)
weight.range <- range(PlantGrowth$weight)

test_that("boxes without coord_flip()", {
  info <- expect_traces(boxes, 3, "boxes")
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }
})

test_that("boxes with facet_grid", {
  facets <- boxes + facet_grid(. ~ type)
  info <- expect_traces(facets, 3, "boxes-facet-grid")
  ## TODO: expect boxes of equal size.

  ## TODO: expect empty space.
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }
})

test_that('boxes with facet_grid(scales="free")', {
  facets.scales <- boxes + facet_grid(. ~ type, scales="free")
  info <- expect_traces(facets.scales, 3, "boxes-scales-free")
  ## TODO: expect boxes of unequal size.

  ## TODO: expect no empty space.
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }
})

test_that('boxes with facet_grid(scales="free", space="free")', {
  facets.space <- boxes + facet_grid(. ~ type, scales="free", space="free")
  info <- expect_traces(facets.space, 3, "boxes-space-free")
  ## TODO: expect boxes of equal size.

  ## TODO: expect no empty space.
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }
})

flipped <- boxes + coord_flip()

test_that("boxes with coord_flip()", {
  info <- expect_traces(flipped, 3, "flip")
  for(tr in info$traces){
    expect_true(is.null(tr[["y"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["x"]]
    expect_equal(computed, expected)
  }
})

## coord_flip + facets are not really even supported in ggplot2, so
## these tests are disabled for now.

test_that("boxes with coord_flip()+facet_grid()", {
  flip.facet <- flipped + facet_grid(type ~ .)
  ##info <- expect_traces(flip.facet, 3)
  ## for(tr in info$traces){
  ##   expect_true(is.null(tr[["y"]]))
  ##   expected <- plant.list[[tr$name]]$weight
  ##   computed <- tr[["x"]]
  ##   expect_equal(computed, expected)
  ## }
})

test_that('boxes with coord_flip()+facet_grid(scales="free")', {
  flip.facet.scales <- flipped + facet_grid(type ~ ., scales="free")
  ##info <- expect_traces(flip.facet.scales, 3)
  ## for(tr in info$traces){
  ##   expect_true(is.null(tr[["y"]]))
  ##   expected <- plant.list[[tr$name]]$weight
  ##   computed <- tr[["x"]]
  ##   expect_equal(computed, expected)
  ## }
})

test_that('boxes+coord_flip()+facet_grid(scales="free", space="free")', {
  flip.facet.space <- flipped +
    facet_grid(type ~ ., scales="free", space="free")
  ## BUG in ggplot2!
})

test_that('boxes+facet_grid(scales="free", space="free")+coord_flip()', {
  flip.facet.space <- boxes +
    facet_grid(type ~ ., scales="free", space="free")+
    coord_flip()
  ## BUG in ggplot2!
})

test_that("Manually set the order of a discrete-valued axis", {
  expected.order <- c("trt1", "ctrl", "trt2")
  boxes.limits <- boxes + scale_x_discrete(limits=expected.order)
  info <- expect_traces(boxes.limits, 3, "discrete-order")
  computed.order <- sapply(info$traces, "[[", "name")
  expect_identical(as.character(computed.order), expected.order)
})

test_that("limits can hide data", {
  expected.order <- c("trt1", "ctrl")
  boxes.limits <- boxes + scale_x_discrete(limits=expected.order)
  info <- expect_traces(boxes.limits, 2, "limits-hide")
  computed.order <- sapply(info$traces, "[[", "name")
  expect_identical(as.character(computed.order), expected.order)
})

test_that("limits can create a gap", {
  expected.order <- c("trt1", "trt2", "GAP", "ctrl")
  boxes.limits <- boxes + scale_x_discrete(limits=expected.order)
  info <- expect_traces(boxes.limits, 3, "limits-gap")
  computed.order <- sapply(info$traces, "[[", "name")
  ##expect_identical(as.character(computed.order), expected.order)

  ## TODO: can we make this in plotly?
})

boxes.breaks <- boxes +
  scale_x_discrete(breaks=c("trt1", "ctrl", "trt2"))

test_that("setting breaks does not change order", {
  info <- expect_traces(boxes.breaks, 3, "breaks-nochange")
  computed.labels <- sapply(info$traces, "[[", "name")
  expect_identical(as.character(computed.labels), c("ctrl", "trt1", "trt2"))
})

boxes.more <- boxes +
  scale_x_discrete(breaks=c("trt1", "ctrl", "trt2", "FOO"))

test_that("more breaks is fine", {
  info <- expect_traces(boxes.more, 3, "breaks-more")
  computed.labels <- sapply(info$traces, "[[", "name")
  expect_identical(as.character(computed.labels), c("ctrl", "trt1", "trt2"))
})

boxes.less <- boxes +
  scale_x_discrete(breaks=c("trt1", "ctrl"))

test_that("less breaks is fine", {
  info <- expect_traces(boxes.less, 3, "breaks-less")
  computed.labels <- sapply(info$traces, "[[", "name")
  ##expect_identical(as.character(computed.labels), c("ctrl", "trt1", "trt2"))

  ## TODO: can we make this in plotly?
})

boxes.labels <- boxes +
  scale_x_discrete(breaks=c("trt1", "ctrl", "trt2"),
                   labels=c("Treatment 1", "Control", "Treatment 2"))

test_that("scale(labels) changes trace names", {
  info <- expect_traces(boxes.labels, 3, "scale-labels")
  computed.labels <- sapply(info$traces, "[[", "name")
  expect_identical(as.character(computed.labels),
                   c("Control", "Treatment 1", "Treatment 2"))
})

no.breaks <- boxes + scale_x_discrete(breaks=NULL)

test_that("hide x ticks, lines, and labels", {
  info <- expect_traces(no.breaks, 3, "hide-ticks-lines-labels")
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

test_that("Hide X ticks and labels, but keep the gridlines", {
  boxes.grid <- boxes +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  info <- expect_traces(boxes.grid, 3, "hide-ticks-labels")
  x <- info$kwargs$layout$xaxis
  expect_identical(x[["showticklabels"]], FALSE)
  expect_identical(x[["showgrid"]], TRUE)
  expect_identical(x[["ticks"]], "")
})

test_that("scale_y_continuous(limits) means yaxis$ranges", {
  boxes.range <- boxes + scale_y_continuous(limits=c(0,8))
  info <- expect_traces(boxes.range, 3, "ycontinuous-ranges")
  y.axis <- info$kwargs$layout$yaxis
  expect_equal(y.axis$range, c(0, 8))
})

test_that("ylim() means yaxis$ranges", {
  boxes.range <- boxes + ylim(0,8)
  info <- expect_traces(boxes.range, 3, "ylim-ranges")
  y.axis <- info$kwargs$layout$yaxis
  expect_equal(y.axis$range, c(0, 8))
  ## ensure correct positive values without reverse scale.
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }  
})

test_that("scale_y_reverse() -> yaxis$ranges reversed", {
  boxes.reverse <- boxes + scale_y_reverse()
  info <- expect_traces(boxes.reverse, 3, "yreverse-ranges")
  y.axis <- info$kwargs$layout$yaxis
  expect_that(y.axis$range[2], is_less_than(y.axis$range[1]))
  ## ensure correct positive values, despite the reverse scale.
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }  
})

test_that("scale_y_reverse(limits) -> yaxis$ranges reversed", {
  y.lim <- c(10, -2)
  boxes.reverse <- boxes + scale_y_reverse(limits=y.lim)
  info <- expect_traces(boxes.reverse, 3, "yreverse-limits-ranges")
  y.axis <- info$kwargs$layout$yaxis
  expect_equal(y.axis$range, y.lim)
  ## ensure correct positive values, despite the reverse scale.
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }  
})

test_that("ylim(reversed) -> yaxis$ranges reversed", {
  boxes.reverse <- boxes + ylim(7.5, -1)
  info <- expect_traces(boxes.reverse, 3, "ylim-reversed-ranges")
  y.axis <- info$kwargs$layout$yaxis
  expect_equal(y.axis$range, c(7.5, -1))
  ## ensure correct positive values, despite the reverse scale.
  for(tr in info$traces){
    expect_true(is.null(tr[["x"]]))
    expected <- plant.list[[tr$name]]$weight
    computed <- tr[["y"]]
    expect_equal(computed, expected)
  }  
})

test_that("Set the X tick mark locations", {
  ## This will show tick marks on every 0.25 from 1 to 10. The scale will
  ## show only the ones that are within range (3.50-6.25 in this case)
  boxes.ticks <- boxes + scale_y_continuous(breaks=seq(1,10,1/4))
  info <- expect_traces(boxes.ticks, 3, "evenly-spaced-ticks")
  y.axis <- info$kwargs$layout$yaxis
  expect_equal(y.axis$dtick, 0.25)
  expect_identical(y.axis$autotick, FALSE)
})

test_that("The breaks can be spaced unevenly", {
  boxes.uneven <- boxes +
    scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6,8))
  ##TODO: is this possible in plotly?
  ## https://plot.ly/python/reference/#YAxis
})

test_that("hide y ticks, lines, and labels", {
  no.breaks <- boxes + scale_y_continuous(breaks=NULL)
  info <- expect_traces(no.breaks, 3, "hide-y")
  y.axis <- info$kwargs$layout$yaxis
  expect_identical(y.axis[["showgrid"]], FALSE)
  expect_identical(y.axis[["ticks"]], "")
  expect_identical(y.axis[["showticklabels"]], FALSE)
})

test_that("hide y ticks and labels, but keep the gridlines", {
  boxes.ygrid <- boxes +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  info <- expect_traces(boxes.ygrid, 3, "hide-y-keep-grid")
  y.axis <- info$kwargs$layout$yaxis
  expect_identical(y.axis[["showgrid"]], TRUE)
  expect_identical(y.axis[["ticks"]], "")
  expect_identical(y.axis[["showticklabels"]], FALSE)
})
