context("Area")

huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
library(plyr) # to access round_any
huron$decade <- round_any(huron$year, 10, floor)

ar <- ggplot(huron) + geom_area(aes(x=year, y=level))
L <- gg2list(ar)

test_that("geom_area is translated to type=area", {
  expect_equal(length(L), 2)
  expect_identical(L[[1]]$type, "scatter")
  expect_equal(L[[1]]$x, c(huron$year[1], huron$year, tail(huron$year, n=1)))
  expect_equal(L[[1]]$y, c(0, huron$level, 0))
})
