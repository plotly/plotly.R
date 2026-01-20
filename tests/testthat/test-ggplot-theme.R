

penguin.base <- ggplot(palmerpenguins::penguins) +
  geom_point(aes(bill_length_mm, flipper_length_mm)) +
  theme_grey()

test_that("background translated correctly",{
  ggpenguin <- penguin.base + 
    theme(panel.background = element_rect(fill = "blue"),
          plot.background = element_rect(fill = "green"))
  info <- expect_doppelganger_built(ggpenguin, "theme-background")
  L <- info$layout
  expect_true(L$plot_bgcolor == toRGB("blue"))
  expect_true(L$paper_bgcolor == toRGB("green"))
})

test_that("grid/ticks translated correctly",{
  ggpenguin <- penguin.base + 
    theme(axis.ticks = element_line(colour = "red"),
          panel.grid.major = element_line(colour = "violet")) 
  info <- expect_doppelganger_built(ggpenguin, "theme-ticks-and-grids")
  for (xy in c("x", "y")) {
    ax.list <- info$layout[[paste0(xy, "axis")]]
    expect_true(ax.list$tickcolor == toRGB("red"))
    expect_true(ax.list$gridcolor == toRGB("violet"))
  }
})

test_that("show ticks as 'outside' by default", {
  ggpenguin <- penguin.base
  info <- expect_doppelganger_built(ggpenguin, "theme-ticks-default")
  for (xy in c("x", "y")) {
    ax.list <- info$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$ticks, "outside")
  }
})

test_that("do not show zeroline by default", {
  ggpenguin <- penguin.base
  info <- expect_doppelganger_built(ggpenguin, "theme-zeroline-default")
  for (xy in c("x", "y")) {
    ax.list <- info$layout[[paste0(xy, "axis")]]
    expect_identical(ax.list$zeroline, FALSE)
  }
})

countrypop <- data.frame(
  country = c("Paraguay", "Peru", "Philippines"),
  population = c(7, 31, 101),
  edu = c(4.2, 1.75, 1.33),
  illn = c(0.38, 1.67, 0.43)
)

gg <- ggplot(countrypop) +
  geom_point(aes(edu, illn, colour = country, size = population))

test_that("marker default shape is a circle", {
  info <- expect_doppelganger_built(gg, "theme-marker-default")
  for (i in c(1:3)) {
    expect_equivalent(info$data[[i]]$marker$symbol, "circle")
    expect_true(info$data[[i]]$showlegend)
  }
})

test_that("plot panel border is translated correctly", {
  ggpenguin <- penguin.base + theme_grey() # has no panel.border
  info <- expect_doppelganger_built(ggpenguin, "theme-panel-border-1")

  red <- ggplot(palmerpenguins::penguins) +
    theme_grey() +
    geom_point(aes(bill_length_mm, bill_depth_mm)) +
    theme(panel.border = element_rect(colour = "red", fill = NA))

  info <- expect_doppelganger_built(red, "theme-panel-border-2")
  expect_true(info$layout$shapes[[1]]$line$color == toRGB("red"))
})

test_that("element_blank panel.border does not create empty shapes (#2455, #2460)", {
  # theme_grey() has element_blank() panel.border - should NOT create a shape
  p_grey <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_grey()
  L_grey <- plotly_build(ggplotly(p_grey))$x
  # No panel border shapes should be created for element_blank
  grey_shapes <- Filter(function(s) identical(s$xref, "paper") && identical(s$yref, "paper"), L_grey$layout$shapes)
  expect_equal(length(grey_shapes), 0)

  # theme_bw() has visible panel.border - SHOULD create a shape
  p_bw <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_bw()
  L_bw <- plotly_build(ggplotly(p_bw))$x
  bw_shapes <- Filter(function(s) identical(s$xref, "paper") && identical(s$yref, "paper"), L_bw$layout$shapes)
  expect_true(length(bw_shapes) > 0)
  expect_false(is.na(bw_shapes[[1]]$line$color))
})

test_that("legend.position is translated correctly (#2407, #2187)", {
  # Test legend.position = "bottom"
  p_bottom <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
    geom_point() +
    theme(legend.position = "bottom")
  L_bottom <- plotly_build(ggplotly(p_bottom))$x
  expect_equal(L_bottom$layout$legend$orientation, "h")
  expect_equal(L_bottom$layout$legend$xanchor, "center")
  expect_equal(L_bottom$layout$legend$y, -0.15)

  # Test legend.position = "top"
  p_top <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
    geom_point() +
    theme(legend.position = "top")
  L_top <- plotly_build(ggplotly(p_top))$x
  expect_equal(L_top$layout$legend$orientation, "h")
  expect_equal(L_top$layout$legend$y, 1.02)

  # Test legend.position = "left"
  p_left <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
    geom_point() +
    theme(legend.position = "left")
  L_left <- plotly_build(ggplotly(p_left))$x
  expect_equal(L_left$layout$legend$xanchor, "right")
  expect_equal(L_left$layout$legend$x, -0.15)

  # Test legend.position = "none"
  p_none <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
    geom_point() +
    theme(legend.position = "none")
  L_none <- plotly_build(ggplotly(p_none))$x
  expect_false(L_none$layout$showlegend)

  # Test numeric legend.position
  p_custom <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
    geom_point() +
    theme(legend.position = c(0.8, 0.2))
  L_custom <- plotly_build(ggplotly(p_custom))$x
  expect_equal(L_custom$layout$legend$x, 0.8)
  expect_equal(L_custom$layout$legend$y, 0.2)
})
