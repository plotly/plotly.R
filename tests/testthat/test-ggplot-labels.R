
test_that("ggtitle is translated correctly", {
  ggpenguin <- ggplot(palmerpenguins::penguins) +
    geom_point(aes(bill_length_mm, bill_depth_mm)) +
    ggtitle("My amazing plot!")
  info <- expect_doppelganger_built(ggpenguin, "labels-ggtitle")
  expect_identical(info$layout$title$text, "My amazing plot!")
})

test_that("ylab is translated correctly", {
  ggpenguin <- ggplot(palmerpenguins::penguins) +
    geom_point(aes(bill_length_mm, bill_depth_mm)) +
    ylab("bill depth")
  info <- expect_doppelganger_built(ggpenguin, "labels-ylab")
  labs <- c(info$layout$xaxis$title$text, info$layout$yaxis$title$text)
  expect_identical(labs, c("bill_length_mm", "bill depth"))
})

test_that("scale_x_continuous(name) is translated correctly", {
  ggpenguin <- ggplot(palmerpenguins::penguins) +
    geom_point(aes(bill_length_mm, bill_depth_mm)) +
    scale_x_continuous("bill length")
  info <- expect_doppelganger_built(ggpenguin, "labels-scale_x_continuous_name")
  labs <- c(info$layout$xaxis$title$text, info$layout$yaxis$title$text)
  expect_identical(labs, c("bill length", "bill_depth_mm"))
})

test_that("angled ticks are translated correctly", {
  ggpenguin <- ggplot(palmerpenguins::penguins) +
    geom_point(aes(bill_length_mm, bill_depth_mm)) +
    theme(axis.text.x = element_text(angle = 45))
  info <- expect_doppelganger_built(ggpenguin, "labels-angles")
  expect_identical(info$layout$xaxis$tickangle, -45)
})

test_that("xaxis/yaxis automargin defaults to TRUE", {
  p <- ggplot(palmerpenguins::penguins, aes(species)) + geom_bar() + coord_flip()
  l <- plotly_build(p)$x
  expect_true(l$layout$xaxis$automargin)
  expect_true(l$layout$yaxis$automargin)
})

test_that("factor labels work", {
  p <- ggplot(diamonds, aes(cut)) + 
    geom_bar() + 
    scale_x_discrete("Cut", labels=factor(letters[1:5]))
  b <- expect_doppelganger_built(p, "factor-labels")
})

test_that("empty labels work", {
  p <- ggplot(palmerpenguins::penguins, 
              aes(bill_length_mm, bill_depth_mm, color = species)) + 
    geom_point() + 
    labs(x = NULL, y = NULL)
  b <- expect_doppelganger_built(p, "labs-element-blank")
})
