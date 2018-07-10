context("ggplot ticks")

PlantGrowth$type <-
  ifelse(PlantGrowth$group == "ctrl", "control", "treatment")
boxes <- ggplot(PlantGrowth, aes(x = group, y = weight)) + geom_boxplot()

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("ticks-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}


test_that("boxes without coord_flip()", {
  info <- expect_traces(boxes, 1, "boxes")
})

test_that("boxes with facet_grid", {
  facets <- boxes + facet_grid(. ~ type)
  info <- expect_traces(facets, 2, "boxes-facet-grid")
})

test_that('boxes with facet_grid(scales="free")', {
  facets.scales <- boxes + facet_grid(. ~ type, scales = "free")
  info <- expect_traces(facets.scales, 2, "boxes-scales-free")
})

test_that('boxes with facet_grid(scales="free", space="free")', {
  ## TODO: implement space free!
  facets.space <- boxes + facet_grid(. ~ type, scales = "free", space = "free")
  info <- expect_traces(facets.space, 2, "boxes-space-free")
})

flipped <- boxes + coord_flip()

test_that("boxes with coord_flip()", {
  info <- expect_traces(flipped, 1, "flip")
})

test_that("boxes with coord_flip()+facet_grid()", {
  flip.facet <- flipped + facet_grid(type ~ .)
  info <- expect_traces(flip.facet, 2, "flip-grid")
})

test_that('boxes with coord_flip()+facet_grid(scales="free")', {
  # bug in ggplot2?
  flip.facet.scales <- flipped + facet_grid(type ~ ., scales = "free")
  info <- expect_traces(flip.facet.scales, 2, "flip-grid-free")
})

test_that("limits can hide data", {
  boxes.limits <- boxes + scale_x_discrete(limits = c("trt1", "ctrl"))
  info <- expect_traces(boxes.limits, 1, "limits-hide")
  expect_equivalent(info$layout$xaxis$ticktext, c("trt1", "ctrl"))
})

test_that("limits can create a gap", {
  boxes.limits <- boxes + scale_x_discrete(limits = c("trt1", "trt2", "GAP", "ctrl"))
  info <- expect_traces(boxes.limits, 1, "limits-gap")
  expect_equivalent(info$layout$xaxis$ticktext, c("trt1", "trt2", "GAP", "ctrl"))
})

boxes.breaks <- boxes +
  scale_x_discrete(breaks = c("trt1", "ctrl", "trt2"))

test_that("setting breaks does not change order", {
  info <- expect_traces(boxes.breaks, 1, "breaks-nochange")
  expect_identical(
    info$layout$xaxis$ticktext[info$layout$xaxis$tickvals], 
    c("ctrl", "trt1", "trt2")
  )
})

boxes.more <- boxes +
  scale_x_discrete(breaks = c("trt1", "ctrl", "trt2", "FOO"))

test_that("more breaks is fine", {
  info <- expect_traces(boxes.more, 1, "breaks-more")
  expect_identical(
    info$layout$xaxis$ticktext[info$layout$xaxis$tickvals], 
    c("ctrl", "trt1", "trt2")
  )
})

boxes.less <- boxes +
  scale_x_discrete(breaks=c("trt1", "ctrl"))

test_that("less breaks is fine", {
  info <- expect_traces(boxes.less, 1, "breaks-less")
  expect_equivalent(info$layout$xaxis$ticktext, c("trt1", "ctrl"))
})

boxes.labels <- boxes +
  scale_x_discrete(breaks=c("trt1", "ctrl", "trt2"),
                   labels=c("Treatment 1", "Control", "Treatment 2"))

test_that("scale(labels) changes trace names", {
  info <- expect_traces(boxes.labels, 1, "scale-labels")
  expect_equivalent(
    info$layout$xaxis$ticktext, 
    c("Treatment 1", "Control", "Treatment 2")
  )
})

no.breaks <- boxes + scale_x_discrete(breaks = NULL)

test_that("hide x ticks, lines, and labels", {
  info <- expect_traces(no.breaks, 1, "hide-ticks-lines-labels")
  expect_true(
    is.na(info$layout$xaxis$ticktext) || length(info$layout$xaxis$ticktext) == 0
  )
  expect_true(
    is.na(info$layout$xaxis$tickvals) || length(info$layout$xaxis$tickvals) == 0
  )
})

test_that("Hide X ticks and labels, but keep the gridlines", {
  boxes.grid <- boxes +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  info <- expect_traces(boxes.grid, 1, "hide-ticks-labels")
  x <- info$layout$xaxis
  expect_false(x$showticklabels)
  expect_true(x$showgrid)
  expect_true(length(x$ticktext) == 3)
})

test_that("scale_y_continuous(limits) means yaxis$ranges", {
  boxes.range <- boxes + scale_y_continuous(limits = c(0,8))
  info <- expect_traces(boxes.range, 1, "ycontinuous-ranges")
  y.axis <- info$layout$yaxis
  expect_equivalent(range(y.axis$tickvals), c(0, 8))
})

test_that("ylim() means yaxis$ranges", {
  boxes.range <- boxes + ylim(0, 8)
  info <- expect_traces(boxes.range, 1, "ylim-ranges")
  y.axis <- info$layout$yaxis
  expect_equivalent(range(y.axis$tickvals), c(0, 8))
})

test_that("scale_y_reverse() -> yaxis$ranges reversed", {
  boxes.reverse <- boxes + scale_y_reverse()
  info <- expect_traces(boxes.reverse, 1, "yreverse-ranges")
})

test_that("scale_y_reverse(limits) -> yaxis$ranges reversed", {
  boxes.reverse <- boxes + scale_y_reverse(limits = c(10, -2))
  info <- expect_traces(boxes.reverse, 1, "yreverse-limits-ranges")
})

test_that("ylim(reversed) -> yaxis$ranges reversed", {
  boxes.reverse <- boxes + ylim(7.5, -1)
  info <- expect_traces(boxes.reverse, 1, "ylim-reversed-ranges")
})

test_that("Set the X tick mark locations", {
  ## This will show tick marks on every 0.25 from 1 to 10. The scale will
  ## show only the ones that are within range (3.50-6.25 in this case)
  boxes.ticks <- boxes + scale_y_continuous(breaks = seq(4, 5, length.out = 12))
  info <- expect_traces(boxes.ticks, 1, "evenly-spaced-ticks")
  y.axis <- info$layout$yaxis
  expect_equivalent(length(y.axis$ticktext), 12)
})

test_that("The breaks can be spaced unevenly", {
  boxes.uneven <- boxes +
    scale_y_continuous(breaks = c(4, 4.25, 4.5, 5, 6, 8))
  info <- expect_traces(no.breaks, 1, "uneven")
})

test_that("R line breaks are translated to HTML line breaks", {
  skip_if_not_installed("stringr")
  
  df_x <- data.frame(
    x = "this is very loooooooooooong text to illustrate",
    y = 100
  )
  p <- ggplot(aes(x = x, y = y), data = df_x) +
    geom_bar(stat = "identity") +
    scale_x_discrete(labels = function(x) getFromNamespace("str_wrap", "stringr")(x, width = 10))
  info <- expect_traces(p, 1, "line-breaks")
  expect_length(
    strsplit(info$layout$xaxis$ticktext, "<br />", fixed = T)[[1]], 5
  )
})


