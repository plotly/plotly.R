context("plotly-linetype")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-linetype-", name))
  expect_equivalent(length(L$data), n.traces)
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
  expect_equivalent(length(dashes), 5)
})

test_that("Can set the linetype range.", {
  p <- plot_ly(tx, x = ~date, y = ~median, linetype = ~city, linetypes = 5:1)
  l <- expect_traces(p, 5, "linetype2")
  lines <- lapply(l$data, "[[", "line")
  dashes <- unlist(lapply(lines, "[[", "dash"))
  expect_equivalent(dashes, plotly:::lty2dash(5:1))
})

test_that("Can avoid scaling", {
  p <- plot_ly(tx, x = ~date, y = ~median, linetype = I(3))
  l <- expect_traces(p, 1, "linetype3")
  lines <- lapply(l$data, "[[", "line")
  dashes <- unlist(lapply(lines, "[[", "dash"))
  expect_equivalent(dashes, plotly:::lty2dash(3))
})

test_that("Warn about invalid linetypes", {
  p <- plot_ly(x = 1:2, y = 1:2, linetype = I("DNE"))
  expect_warning(plotly_build(p), "DNE")
})

test_that("Can specify a scale manually", {
  pal <- c("1" = "dot", "0" = "dash")
  p <- plot_ly(mtcars, x = ~mpg, y = ~disp, linetype = ~factor(vs), linetypes = pal)
  l <- expect_traces(p, 2, "manual")
  dashes <- lapply(l$data, "[[", "line")
  expected <- setNames(pal[sapply(l$data, "[[", "name")], NULL)
  expect_equivalent(expected, sapply(dashes, "[[", "dash"))
})

test_that("Trace ordering matches factor levels", {
  p <- plot_ly(mtcars, x = ~mpg, y = ~disp, linetype = ~factor(vs, levels = c(1, 0))) %>% add_lines()
  l <- expect_traces(p, 2, "ordering")
  expect_equivalent(sapply(l$data, "[[", "name"), c("1", "0"))
})

test_that("Trace ordering is alphabetical", {
  lvls <- sort(unique(mpg$class))
  p <- plot_ly(mpg, x = ~cty, y = ~hwy, linetype = ~class) %>% add_lines()
  l <- expect_traces(p, length(lvls), "alphabetical")
  expect_equivalent(sapply(l$data, "[[", "name"), lvls)
})
