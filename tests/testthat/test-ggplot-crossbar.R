context("crossbar")


d <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p <- ggplot(d, aes(trt, resp)) + 
  geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)


test_that("Basic geom_crossbar() works", {
  
  l <- plotly_build(p)$x
  
  # one trace for each rect and one trace for middle segments,
  # but does it _really_ matter?
  expect_length(l$data, 5)
  
})


p <- ggplot(d, aes(trt, resp, color = group, linetype = group)) + 
  geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_colour_manual(values = c("red", "purple"))

test_that("geom_crossbar() with aesthetics", {
  
  l <- plotly_build(p)$x
  
  # one trace for each rect and 2 traces for middle segments...
  expect_length(l$data, 6)
  
  colors <- vapply(l$data, function(x) x$line$color, character(1))
  dashes <- vapply(l$data, function(x) x$line$dash, character(1))
  
  expect_equivalent(
    unique(colors), toRGB(c("red", "purple"))
  )
  
  expect_equivalent(
    unique(dashes), lty2dash(1:2)
  )
  
})
