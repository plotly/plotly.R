context("text")

test_that("label is translated correctly",{
  gg <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) + geom_text()
  info <- gg2list(gg)
  expect_identical(as.character(info[[1]]$text), rownames(mtcars))
})

test_that("position is translated correctly",{
  gg <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) + geom_text()
  info <- gg2list(gg)
  expect_identical(info[[1]]$x, mtcars$wt)
  expect_identical(info[[1]]$y, mtcars$mpg)
})

test_that("textsize is translated correctly",{
  gg <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) + 
    geom_text(size=18)
  info <- gg2list(gg)
  expect_identical(info[[1]]$textfont$size, 18)
})
