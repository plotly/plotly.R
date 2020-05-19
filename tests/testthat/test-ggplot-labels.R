context("labels")

test_that("ggtitle is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    ggtitle("My amazing plot!")
  info <- expect_doppelganger_built(ggiris, "labels-ggtitle")
  expect_identical(info$layout$title$text, "My amazing plot!")
})

test_that("ylab is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    ylab("sepal width")
  info <- expect_doppelganger_built(ggiris, "labels-ylab")
  labs <- c(info$layout$xaxis$title$text, info$layout$yaxis$title$text)
  expect_identical(labs, c("Petal.Width", "sepal width"))
})

test_that("scale_x_continuous(name) is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    scale_x_continuous("petal width")
  info <- expect_doppelganger_built(ggiris, "labels-scale_x_continuous_name")
  labs <- c(info$layout$xaxis$title$text, info$layout$yaxis$title$text)
  expect_identical(labs, c("petal width", "Sepal.Width"))
})

test_that("angled ticks are translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    theme(axis.text.x = element_text(angle = 45))
  info <- expect_doppelganger_built(ggiris, "labels-angles")
  expect_identical(info$layout$xaxis$tickangle, -45)
})

test_that("xaxis/yaxis automargin defaults to TRUE", {
  p <- ggplot(iris, aes(Species)) + geom_bar() + coord_flip()
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
  p <- ggplot(iris, aes(Petal.Length, Sepal.Width, color = Species)) + 
    geom_point() + 
    labs(x = element_blank(), y = element_blank())
  b <- expect_doppelganger_built(p, "labs-element-blank")
})
