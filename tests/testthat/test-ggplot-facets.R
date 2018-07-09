context("Facets")

test_that("6 facets becomes 6 panels", {
  
  # data(barley, package = "lattice")
  # dput(barley)
  barley <- structure(list(
    yield = c(27, 48.86667, 27.43334, 39.93333, 32.96667, 28.96667, 43.06666, 55.2, 28.76667, 38.13333, 29.13333, 29.66667, 35.13333, 47.33333, 25.76667, 40.46667, 29.66667, 25.7, 39.9, 50.23333, 26.13333, 41.33333, 23.03333, 26.3, 36.56666, 63.8333, 43.76667, 46.93333, 29.76667, 33.93333, 43.26667, 58.1, 28.7, 45.66667, 32.16667, 33.6, 36.6, 65.7667, 30.36667, 48.56666, 24.93334, 28.1, 32.76667, 48.56666, 29.86667, 41.6, 34.7, 32, 24.66667, 46.76667, 22.6, 44.1, 19.7, 33.06666, 39.3, 58.8, 29.46667, 49.86667, 34.46667, 31.6, 26.9, 33.46667, 34.36666, 32.96667, 22.13333, 22.56667, 36.8, 37.73333, 35.13333, 26.16667, 14.43333, 25.86667, 27.43334, 38.5, 35.03333, 20.63333, 16.63333, 22.23333, 26.8, 37.4, 38.83333, 32.06666, 32.23333, 22.46667, 29.06667, 49.2333, 46.63333, 41.83333, 20.63333, 30.6, 26.43334, 42.2, 43.53334, 34.33333, 19.46667, 22.7, 25.56667, 44.7, 47, 30.53333, 19.9, 22.5, 28.06667, 36.03333, 43.2, 25.23333, 26.76667, 31.36667, 30, 41.26667, 44.23333, 32.13333, 15.23333, 27.36667, 38, 58.16667, 47.16667, 35.9, 20.66667, 29.33333), 
    variety = structure(c(3L, 3L, 3L, 3L, 3L, 3L, 7L, 7L, 7L, 7L, 7L, 7L, 1L, 1L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 5L, 5L, 5L, 10L, 10L, 10L, 10L, 10L, 10L, 8L, 8L, 8L, 8L, 8L, 8L, 2L, 2L, 2L, 2L, 2L, 2L, 6L, 6L, 6L, 6L, 6L, 6L, 4L, 4L, 4L, 4L, 4L, 4L, 9L, 9L, 9L, 9L, 9L, 9L, 3L, 3L, 3L, 3L, 3L, 3L, 7L, 7L, 7L, 7L, 7L, 7L, 1L, 1L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 5L, 5L, 5L, 10L, 10L, 10L, 10L, 10L, 10L, 8L, 8L, 8L, 8L, 8L, 8L, 2L, 2L, 2L, 2L, 2L, 2L, 6L, 6L, 6L, 6L, 6L, 6L, 4L, 4L, 4L, 4L, 4L, 4L, 9L, 9L, 9L, 9L, 9L, 9L), 
                        .Label = c("Svansota", "No. 462", "Manchuria", "No. 475", "Velvet", "Peatland", "Glabron", "No. 457", "Wisconsin No. 38", "Trebi"), class = "factor"), 
    year = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("1932", "1931"), class = "factor"), 
    site = structure(c(3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L, 3L, 6L, 4L, 5L, 1L, 2L), .Label = c("Grand Rapids", "Duluth", "University Farm", "Morris", "Crookston", "Waseca"), class = "factor")), 
    row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100", "101", "102", "103", "104", "105", "106", "107", "108", "109", "110", "111", "112", "113", "114", "115", "116", "117", "118", "119", "120"), 
    class = "data.frame"
  )
  gg <- qplot(yield, variety, data = barley, 
              color = year, facets = site ~ ., pch = I(1))+
    theme_bw() +
    theme(panel.spacing = grid::unit(0, "cm"))
  info <- save_outputs(gg, "barley")
  # two legend entries, but two groups
  expect_equivalent(sum(sapply(info$data, "[[", "showlegend")), 2)
  expect_identical(
    sort(unique(sapply(info$data, "[[", "legendgroup"))), c("1931", "1932")
  )
  expect_identical(
    sort(unique(sapply(info$data, "[[", "name"))), c("1931", "1932")
  )
})

test_that("3 facets becomes 3 panels", {
  df <- data.frame(
    x = runif(99), 
    y = runif(99), 
    z = rep(c('a','b','c'), 33)
  )
  gg <- qplot(x, y, data = df, facets = z ~ ., pch = I(1)) +
    theme_bw() +
    theme(panel.spacing = grid::unit(0, "cm"))
  info <- save_outputs(gg, "3-panels")
  yaxes <- sapply(info$data, "[[", "yaxis")
  xaxes <- sapply(info$data, "[[", "xaxis")
  expect_true(all(c("y", "y2", "y3") %in% yaxes))
  expect_true(all(xaxes == "x"))
})

# expect a certain number of _unique_ [x/y] axes
expect_axes <- function(info, n, axis = "x") {
  pattern <- paste0("^", axis, "axis([0-9]+)?$")
  axes <- with(info, layout[grepl(pattern, names(layout))])
  n.axes <- length(axes)
  ranges <- do.call("rbind", lapply(axes, function(x) x$range))
  expect_identical(nrow(unique(ranges)), as.integer(n))
}

no_panels <- ggplot(mtcars, aes(mpg, wt)) + geom_point()

test_that("facet_wrap(..., scales = 'free') creates interior scales", {
  free_both <- no_panels + facet_wrap(~ am + vs, scales = "free")
  info <- save_outputs(free_both, "facet_wrap_free")
  expect_axes(info, 4L)
  expect_axes(info, 4L, "y")
  
  free_y <- no_panels + facet_wrap(~am+vs, scales = "free_y")
  info <- save_outputs(free_y, "facet_wrap_free_y")
  expect_axes(info, 1L)
  expect_axes(info, 4L, "y")
  
  free_x <- no_panels + facet_wrap(~am+vs, scales = "free_x")
  info <- save_outputs(free_x, "facet_wrap_free_x")
  expect_axes(info, 4L)
  expect_axes(info, 1L, "y")
})

test_that("facet_grid(..., scales = 'free') doesnt create interior scales.", {
  free_both <- no_panels + facet_grid(vs ~ am, scales = "free")
  info <- save_outputs(free_both, "facet_grid_free")
  expect_axes(info, 2L)
  expect_axes(info, 2L, "y")
  
  free_y <- no_panels + facet_grid(vs~am, scales = "free_y")
  info <- save_outputs(free_y, "facet_grid_free_y")
  expect_axes(info, 1L)
  expect_axes(info, 2L, "y")
  
  free_x <- no_panels + facet_grid(vs~am, scales = "free_x")
  info <- save_outputs(free_x, "facet_grid_free_x")
  expect_axes(info, 2L)
  expect_axes(info, 1L, "y")
})

gg <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point() + geom_line() + 
  facet_wrap(~ cyl, scales = "free", ncol = 2)

test_that("facet_wrap(..., scales = 'free') can handle multiple traces on each panel", {
  info <- save_outputs(gg, "facet_wrap_free_mult")
  yaxes <- unique(sapply(info$data, "[[", "yaxis"))
  for (i in yaxes) {
    dat <- info$data[sapply(info$data, "[[", "yaxis") %in% i]
    modes <- sort(sapply(dat, "[[", "mode"))
    expect_true(all(modes %in% c("lines", "markers")))
  }
})

test_that("facet_wrap() doesn't create interior scales", {
  g <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_wrap(~cyl)
  info <- save_outputs(g, "facet_wrap")
  expect_equivalent(unique(unlist(lapply(info$data, "[[", "yaxis"))), "y")
})


g <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point() +
  facet_wrap( ~ am, labeller = label_both)

test_that("facet_wrap translates simple labeller function", {
  info <- save_outputs(g, "facet_wrap-labeller")
  txt <- sapply(info$layout$annotations, "[[", "text")
  expect_true(all(c("am: 0", "am: 1") %in% txt))
})

g <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point() +
  facet_grid(vs ~ am, labeller = label_both)

test_that("facet_grid translates simple labeller function", {
  info <- save_outputs(g, "facet_grid-labeller")
  txt <- sapply(info$layout$annotations, "[[", "text")
  expect_true(
    all(c("am: 0", "am: 1", "vs: 0", "vs: 1") %in% txt)
  )
})

p <- economics %>% tidyr::gather(variable, value, -date) %>% 
  qplot(data = ., date, value) + 
  facet_wrap(~variable, scale = "free_y", ncol = 2)

test_that("when y scales are free, x-axes are still anchored on exterior", {
  info <- save_outputs(p, "facet_wrap-free_y")
  xaxes <- info$layout[grep("^xaxis", names(info$layout))]
  yaxes <- info$layout[grep("^yaxis", names(info$layout))]
  expect_equivalent(unique(sapply(xaxes, "[[", "anchor")), "y5")
})
