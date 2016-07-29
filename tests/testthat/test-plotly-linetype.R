context("plotly-linetype")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equal(length(L$data), n.traces)
  L
}

# subset to five cities
cities <- unique(txhousing$city)[1:5]
tx <- txhousing[txhousing$city %in% cities, ]
tx$city_ <- match(tx$city, cities)

test_that("Mapping a variable to linetype works", {
  p <- plot_ly(tx, x = ~date, y = ~median, linetype = ~city)
  l <- expect_traces(p, 5, "linetype")
  lines <- lapply(l$data, "[[", "line")
  dashes <- unlist(lapply(lines, "[[", "dash"))
  expect_equal(length(dashes), 5)
})

test_that("Can set the linetype range.", {
  p <- plot_ly(tx, x = ~date, y = ~median, linetype = ~city, linetypes = 5:1)
  l <- expect_traces(p, 5, "linetype2")
  lines <- lapply(l$data, "[[", "line")
  dashes <- unlist(lapply(lines, "[[", "dash"))
  expect_equal(dashes, plotly:::lty2dash(5:1))
})

test_that("Can avoid scaling", {
  p <- plot_ly(tx, x = ~date, y = ~median, linetype = I(3))
  l <- expect_traces(p, 1, "linetype3")
  lines <- lapply(l$data, "[[", "line")
  dashes <- unlist(lapply(lines, "[[", "dash"))
  expect_equal(dashes, plotly:::lty2dash(3))
})

test_that("Warn about invalid linetypes", {
  p <- plot_ly(x = 1:2, y = 1:2, linetype = I("DNE"))
  expect_warning(plotly_build(p), "DNE")
})
