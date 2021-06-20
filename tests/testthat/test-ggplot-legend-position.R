content("legend-positioning")

expect_legend <- function(p, name, position = "right") {
  p <- p + theme(legend.position = position)
  name <- paste0(name, "-", position)
  expect_doppelganger_built(p, name)
  p <- p + theme(legend.direction = "horizontal")
  expect_doppelganger_built(p, paste0(name, "-h"))
}

test_that("One legend positioning", {
  one_legend <- ggplot(mtcars) + 
    geom_point(aes(wt, mpg, color = factor(cyl)))
  expect_legend(one_legend, "one-legend", "right")
  expect_legend(one_legend, "one-legend", "left")
  expect_legend(one_legend, "one-legend", "top")
  expect_legend(one_legend, "one-legend", "bottom")
})

test_that("One colorbar positioning", {
  one_colorbar <- ggplot(mtcars) + 
    geom_point(aes(wt, mpg, color = mpg))
  expect_legend(one_colorbar, "one-colorbar", "right")
  expect_legend(one_colorbar, "one-colorbar", "left")
  expect_legend(one_colorbar, "one-colorbar", "top")
  expect_legend(one_colorbar, "one-colorbar", "bottom")
})


test_that("One legend & one colorbar positioning", {
  both <- ggplot(mtcars) + 
    geom_point(aes(wt, mpg, color = mpg, shape = factor(cyl)))
  expect_legend(both, "both", "right")
  expect_legend(both, "both", "left")
  expect_legend(both, "both", "top")
  expect_legend(both, "both", "bottom")
})



