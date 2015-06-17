context("names")

test_that("name param is passed to plotly", {
  library(maps)
  data(canada.cities)
  gg <- ggplot(canada.cities, aes(long, lat)) +
    borders(regions="canada", name="borders") +
    geom_point(aes(text=name, size=pop), colour="red",
               alpha=1/2, pch=1, name="cities")
  info <- save_outputs(gg, "names")
  expect_identical(info$data[[1]]$name, "borders")
  expect_identical(info$data[[2]]$name, "cities")
})
