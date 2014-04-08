context("polygon")

test_that("polygons become one trace with NA", {
  library(maps)
  data(canada.cities)
  gg <- ggplot(canada.cities, aes(long, lat))+
    borders(regions="canada", name="borders")
  info <- gg2list(gg)
  expect_identical(length(info), 2L)
  tr <- info[[1]]
  expect_true(any(is.na(tr$x)))
})
