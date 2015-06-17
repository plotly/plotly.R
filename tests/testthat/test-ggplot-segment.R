context("segment")

test_that("segments become one path", {
  seg.df <- data.frame(x=c(0, 0),
                       y=c(0, 1),
                       xend=c(1, 1),
                       yend=c(0, 1))
  gg <- ggplot() +
    geom_segment(aes(x, y, xend=xend, yend=yend), data=seg.df)
  info <- save_outputs(gg, "segment")
  tr <- info$data[[1]]
  expect_true(any(is.na(tr$x)))
  expect_true(any(is.na(tr$y)))
})
