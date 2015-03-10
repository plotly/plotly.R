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

rb2 <- ggplot(huron, aes(x=year)) + 
  geom_ribbon(aes(ymin=level-1, ymax=level+1), alpha = 0.1)
L2 <- gg2list(rb2)

test_that("geom_ribbon respects alpha transparency", {
  expect_match(L2[[1]]$fillcolor, "0.1)", fixed=TRUE)
})

save_outputs(rb2, "ribbon-alpha")
