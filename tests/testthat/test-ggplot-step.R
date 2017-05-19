context("Step")

# Dataset for test
oranges <- subset(Orange, Tree %in% c(1, 2))
# Line shapes available in plotly: "linear", "spline", "hv", "vh", "hvh", "vhv"

gg <- ggplot(oranges, aes(x=age, y=circumference,
                          group=Tree, colour=factor(Tree)))

test_that("direction hv is translated to shape=hv", {
  gg.hv <- gg + geom_step()
  L <- save_outputs(gg.hv, "step-gg.hv")
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "hv")
})

test_that("direction vh is translated to shape=vh", {
  gg.vh <- gg + geom_step(direction = "vh")
  L <- save_outputs(gg.vh, "step-gg.vh")
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "vh")
})

test_that("direction hvh is translated to shape=hvh", {
  gg.hvh <- gg + geom_step(direction="hvh")
  L <- save_outputs(gg.hvh, "step-gg.hvh")
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "hvh")
})

test_that("direction vhv is translated to shape=vhv", {
  gg.vhv <- gg + geom_step(direction="vhv")
  L <- save_outputs(gg.vhv, "step-gg.vhv")
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "vhv")
})
