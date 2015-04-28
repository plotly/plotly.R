context("Step")

# Dataset for test
oranges <- subset(Orange, Tree %in% c(1, 2))
# Line shapes available in plotly: "linear", "spline", "hv", "vh", "hvh", "vhv"

gg <- ggplot(oranges, aes(x=age, y=circumference,
                          group=Tree, colour=factor(Tree)))

test_that("direction of geom_line is translated to shape=linear", {
  gg.linear <- gg + geom_line()
  L <- gg2list(gg.linear)
  expect_equal(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "linear")

  save_outputs(gg.linear, "step-gg.linear-geom_line")
})

test_that("direction of geom_path is translated to shape=linear", {
  gg.lin <- gg + geom_path()
  L <- gg2list(gg.lin)
  expect_equal(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "linear")

  save_outputs(gg.lin, "step-gg.linear-geom_path")
})

test_that("direction hv is translated to shape=hv", {
  gg.hv <- gg + geom_step()
  L <- gg2list(gg.hv)
  expect_equal(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "hv")

  save_outputs(gg.hv, "step-gg.hv")
})

test_that("direction vh is translated to shape=vh", {
  gg.vh <- gg + geom_step(direction="vh")
  L <- gg2list(gg.vh)
  expect_equal(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "vh")

  save_outputs(gg.vh, "step-gg.vh")
})

test_that("direction hvh is translated to shape=hvh", {
  gg.hvh <- gg + geom_step(direction="hvh")
  L <- gg2list(gg.hvh)
  expect_equal(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "hvh")

  save_outputs(gg.hvh, "step-gg.hvh", TRUE)
})

test_that("direction vhv is translated to shape=vhv", {
  gg.vhv <- gg + geom_step(direction="vhv")
  L <- gg2list(gg.vhv)
  expect_equal(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "vhv")

  save_outputs(gg.vhv, "step-gg.vhv", TRUE)
})
