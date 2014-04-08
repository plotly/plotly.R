context("path")

df <- data.frame(x=c(1, 3, 2),
                 y=c(0, 0, 1))

test_that("lines are different from paths", {
  p <- qplot(x, y, data=df, geom="path")
  p.tr <- gg2list(p)[[1]]
  expect_identical(p.tr$x, c(1, 3, 2))
  expect_identical(p.tr$y, c(0, 0, 1))
  l <- qplot(x, y, data=df, geom="line")
  l.tr <- gg2list(l)[[1]]
  expect_identical(l.tr$x, c(1, 2, 3))
  expect_identical(l.tr$y, c(0, 1, 0))
})
