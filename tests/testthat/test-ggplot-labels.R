context("labels")

test_that("ggtitle is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    ggtitle("My amazing plot!")
  info <- save_outputs(ggiris, "labels-ggtitle")
  expect_identical(info$layout$title, "My amazing plot!")
})

test_that("ylab is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    ylab("sepal width")
  info <- save_outputs(ggiris, "labels-ylab")
  labs <- c(info$layout$xaxis$title, info$layout$yaxis$title)
  expect_identical(labs, c("Petal.Width", "sepal width"))
})

# TODO: why is this failing on R-devel???
#test_that("scale_x_continuous(name) is translated correctly", {
#  ggiris <- ggplot(iris) +
#    geom_point(aes(Petal.Width, Sepal.Width)) +
#    scale_x_continuous("petal width")
#  info <- save_outputs(ggiris, "labels-scale_x_continuous_name")
#  labs <- unlist(lapply(info$layout$annotations, "[[", "text"))
#  expect_identical(sort(labs), c("petal width", "Sepal.Width"))
#})

test_that("angled ticks are translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    theme(axis.text.x = element_text(angle = 45))
  info <- save_outputs(ggiris, "labels-angles")
  expect_identical(info$layout$xaxis$tickangle, -45)
})

test_that("xaxis/yaxis automargin defaults to TRUE", {
  p <- ggplot(iris, aes(Species)) + geom_bar() + coord_flip()
  l <- plotly_build(p)$x
  expect_true(l$layout$xaxis$automargin)
  expect_true(l$layout$yaxis$automargin)
})
