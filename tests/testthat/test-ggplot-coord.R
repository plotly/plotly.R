context("coord")

base <- qplot(mpg, wt, data = mtcars) 
p <- base + coord_fixed()

test_that("simple fixed coordinates", {
  l <- save_outputs(p, "coord-fixed")
  # I don't think there's a good way to test ratios explictly...
  # we'll rely on visual testing for now
})

base2 <- qplot(wt, mpg, data = mtcars)
p <- base2 + coord_fixed()

test_that("simple fixed coordinates", {
  l <- save_outputs(p, "coord-fixed2")
  # I don't think there's a good way to test ratios explictly...
  # we'll rely on visual testing for now
})


p <- base +
  facet_grid(vs ~ am, labeller = label_both) + 
  coord_fixed() + ylim(0, 5)

test_that("fixed coordinates with facets", {
  l <- save_outputs(p, "coord-fixed-facet")
})

p <- base2 +
  facet_grid(vs ~ am, labeller = label_both) + 
  coord_fixed()

test_that("fixed coordinates with facets", {
  l <- save_outputs(p, "coord-fixed-facet2")
})

p <- qplot(1:10, rep(1:2, 5), colour = sapply(11:20, function(x) paste(rep("a", x), collapse = ""))) + 
  coord_fixed() + scale_color_discrete("aslk")

test_that("fixed coordinates with long legend entries", {
  l <- save_outputs(p, "coord-fixed-long-entries")
})

p <- qplot(1:10, rep(1:2, 5), colour = factor(1:10)) + 
  coord_fixed() + scale_color_discrete("aslkdsadklnasn\nsa;mkdas;dm")

test_that("fixed coordinates with long legend title", {
  l <- save_outputs(p, "coord-fixed-long-title")
})
