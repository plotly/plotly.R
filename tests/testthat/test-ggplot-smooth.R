context("smooth")

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + geom_smooth()

test_that("geom_point() + geom_smooth() produces 3 traces", {
  info <- gg2list(p)
  expect_true(sum(names(info) == "") == 3)
  save_outputs(p, "smooth")
})

p2 <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + geom_smooth(se = FALSE)

test_that("geom_point() + geom_smooth(se = FALSE) produces 2 traces", {
  info2 <- gg2list(p2)
  expect_true(sum(names(info2) == "") == 2)
  save_outputs(p2, "smooth-se-false")
})

