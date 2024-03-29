

# Dataset for test
oranges <- subset(Orange, Tree %in% c(1, 2))
# Line shapes available in plotly: "linear", "spline", "hv", "vh", "hvh", "vhv"

gg <- ggplot(oranges, aes(x=age, y=circumference,
                          group=Tree, colour=factor(Tree)))

test_that("direction hv is translated to shape=hv", {
  gg.hv <- gg + geom_step()
  L <- expect_doppelganger_built(gg.hv, "step-gg.hv")
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "hv")
})

test_that("direction vh is translated to shape=vh", {
  gg.vh <- gg + geom_step(direction = "vh")
  L <- expect_doppelganger_built(gg.vh, "step-gg.vh")
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "vh")
})

test_that("direction hvh is translated to shape=hvh", {
  gg.hvh <- gg + geom_step(direction="hvh")
  L <- expect_doppelganger_built(gg.hvh, "step-gg.hvh")
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "hvh")
})

test_that("direction vhv is translated to shape=vhv", {
  gg.vhv <- gg + geom_step(direction="vhv")
  L <- expect_doppelganger_built(gg.vhv, "step-gg.vhv")
  expect_equivalent(length(L$data), 2)
  expect_identical(L$data[[1]]$line$shape, "vhv")
})


test_that("`stat_ecdf` renders correctly", {
  df <- data.frame(
    x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
    g = gl(2, 100)
  )
  
  p <- ggplot(df, aes(x)) + stat_ecdf(geom = "step")
  expect_doppelganger(ggplotly(p), "step-ecdf")
  
  p <- ggplot(df, aes(x, colour = g)) + stat_ecdf()
  expect_doppelganger(ggplotly(p), "step-ecdf-multiple")
})
