context("segment")

test_that("segments become one path", {
  seg.df <- data.frame(
    x = c(0, 0),
    y = c(0, 1),
    xend = c(1, 1),
    yend = c(0, 1)
  )
  gg <- ggplot() +
    geom_segment(aes(x, y, xend  =  xend, yend = yend), data = seg.df)
  info <- save_outputs(gg, "segment")
  tr <- info$data[[1]]
  expect_true(any(is.na(tr$x)))
  expect_true(any(is.na(tr$y)))
})

test_that("with non-numeric data, we can have more than one segment", {
  df <- data.frame(donation = c(102.35377, 98.80028, 102.34715, 103.71195,
                                107.74814, 92.21549, 103.54709, 93.52689,
                                104.32014, 93.23326, 123.76597, 128.53826,
                                125.36151, 116.29949, 125.65676, 118.60371,
                                117.60477, 128.28911, 121.93446, 127.63119,
                                97.61806, 94.25784, 102.66568, 100.75126,
                                96.08688, 89.15305, 100.29993, 89.76010,
                                103.79008, 96.71342, 95.31541, 107.68345,
                                94.42277, 98.91443, 100.55720, 104.00674,
                                91.39054, 94.11684, 102.08854, 97.04515),
                   campaign = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
                                2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                2, 2, 2, 2, 2, 2))
  
  seg1 <- data.frame(x = 0.85, xend = 1.15, y = 100.2, yend = 100.2)
  seg2 <- data.frame(x = 1.85, xend = 2.15, y = 123.5, yend = 123.5)
  
  gg <- ggplot() +
    geom_point(data = df, aes(x = campaign, y = donation, colour = campaign)) +
    geom_segment(data = seg1, aes(x, y, xend = xend, yend = yend)) +
    geom_segment(data = seg2, aes(x, y, xend = xend, yend = yend))
  
  fig <- save_outputs(gg, "segment-multiple-non-numeric")
  # one trace is for the colorbar
  expect_equivalent(length(fig$data), 4)
  expect_equivalent(fig$data[[2]]$x[1], seg1$x)
  expect_equivalent(fig$data[[2]]$x[2], seg1$xend)
  expect_equivalent(fig$data[[3]]$x[1], seg2$x)
  expect_equivalent(fig$data[[3]]$x[2], seg2$xend)
})
