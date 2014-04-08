context("segment")

test_that("segments become one path", {
  seg.df <- data.frame(x=c(0, 0),
                       y=c(0, 1),
                       xend=c(1, 1),
                       yend=c(0, 1))
  gg <- ggplot()+
    geom_segment(aes(x, y, xend=xend, yend=yend), data=seg.df)
  info <- gg2list(gg)
  tr <- info[[1]]
  expect_equal(tr$x, c(0, 1, NA, 0, 1))
  expect_equal(tr$y, c(0, 0, NA, 1, 1))
})
