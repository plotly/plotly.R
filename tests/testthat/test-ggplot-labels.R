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
  expect_identical(info$layout$xaxis$title, "Petal.Width")
  expect_identical(info$layout$yaxis$title, "sepal width")
})

test_that("scale_x_continuous(name) is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    scale_x_continuous("petal width")
  info <- save_outputs(ggiris, "labels-scale_x_continuous_name")
  expect_identical(info$layout$xaxis$title, "petal width")
  expect_identical(info$layout$yaxis$title, "Sepal.Width")
})

test_that("angled ticks are translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    theme(axis.text.x=element_text(angle=45))
  info <- save_outputs(ggiris, "labels-angles")
  expect_identical(info$layout$xaxis$tickangle, -45)
})

# TODO: test label colors.
