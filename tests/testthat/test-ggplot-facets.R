context("Facets")

test_that("6 facets becomes 6 panels", {
  data(barley, package = "lattice")
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
