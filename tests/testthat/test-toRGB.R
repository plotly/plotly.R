context("toRGB")

test_that("Can apply alpha recursively with toRGB()", {
  col <- toRGB(toRGB("black", 0.9), 0.9)
  expect_match(col, "rgba\\(0,0,0,0\\.80")
})
