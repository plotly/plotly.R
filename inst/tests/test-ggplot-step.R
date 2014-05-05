context("Step")

# Dataset for test
oranges <- subset(Orange, Tree == 1, select=-Tree)
# Line shapes available in plotly
shapes <- c("linear", "spline", "hv", "vh", "hvh", "vhv")

gg <- ggplot(oranges, aes(x=age, y=circumference))

for (d in shapes) {
  test_that("direction is translated to shape (same values)", {
    gg.dir <- gg + geom_step(direction=d)
    L <- gg2list(gg.dir)
    expect_identical(L[[1]]$line$shape, d)
  })
}
