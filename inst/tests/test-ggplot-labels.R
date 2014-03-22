context("labels")

test_that("title is translated correctly", {
  ggiris <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))+
    ggtitle("My amazing plot!")
  info <- gg2list(ggiris)
  expect_identical(info$kwargs$layout$title, "My amazing plot!")
})
