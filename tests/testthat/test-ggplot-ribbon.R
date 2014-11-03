context("ribbon")

huron <- data.frame(year=1875:1972, level=as.vector(LakeHuron))

rb <- ggplot(huron, aes(x=year)) + geom_ribbon(aes(ymin=level-1, ymax=level+1))
L <- gg2list(rb)

test_that("sanity check for geom_ribbon", {
  expect_equal(length(L), 2)
  expect_identical(L[[1]]$type, "scatter")
  expect_equal(L[[1]]$x, c(huron$year[1], huron$year, rev(huron$year)))
  expect_equal(L[[1]]$y, c(huron$level[1]-1, huron$level+1, rev(huron$level-1)))
  expect_identical(L[[1]]$line$color, "transparent")
})

save_outputs(rb, "ribbon")
