context("labels")

test_that("ggtitle is translated correctly", {
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))+
    ggtitle("My amazing plot!")
  info <- gg2list(ggiris)
  expect_identical(info$kwargs$layout$title, "My amazing plot!")
})

test_that("ylab is translated correctly", {
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))+
    ylab("sepal width")
  info <- gg2list(ggiris)
  expect_identical(info$kwargs$layout$xaxis$title, "Petal.Width")
  expect_identical(info$kwargs$layout$yaxis$title, "sepal width")
})

test_that("scale_x_continuous(name) is translated correctly", {
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))+
    scale_x_continuous("petal width")
  info <- gg2list(ggiris)
  expect_identical(info$kwargs$layout$xaxis$title, "petal width")
  expect_identical(info$kwargs$layout$yaxis$title, "Sepal.Width")
})

test_that("angled ticks are translated correctly", {
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))+
    theme(axis.text.x=element_text(angle=45))
  info <- gg2list(ggiris)
  expect_identical(info$kwargs$layout$xaxis$tickangle, -45)
})

##TODO: test label colors.
