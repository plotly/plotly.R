context("plotly data")

test_that("uniq works as expected", {
  expect_equivalent(uniq(c("red", "red", NA)), "red")
})

test_that("plotly_data returns empty data frame when none is specified", {
  d <- plotly_data(plot_ly())
  expect_true(is.data.frame(d) && NROW(d) == 0)
})

test_that("plotly_data returns data frame", {
  d <- plotly_data(plot_ly(economics))
  expect_identical(as.data.frame(economics), as.data.frame(d))
})

test_that("plotly_data preserves groups in data", {
  d <- plotly_data(group_by_(plot_ly(mtcars), c("vs", "am")))
  expect_true(dplyr::groups(d)[[1]] == "vs")
})

test_that("add_fun can apply two different chunks of data to a plot", {
  p <- plot_ly(mtcars, x = ~wt, y = ~mpg) %>%
    add_markers() %>%
    add_fun(function(p) {
      p %>% slice(which.max(mpg)) %>% add_annotations("Good mileage")
    }) %>%
    add_fun(function(p) {
      p %>% slice(which.min(mpg)) %>% add_annotations(text = "Bad mileage")
    })
  l <- plotly_build(p)[["x"]]
  expect_equivalent(length(l$layout$annotations), 2)
  expect_equivalent(
    sort(sapply(l$layout$annotations, "[[", "text")),
    c("Bad mileage", "Good mileage")
  )
  expect_equivalent(
    sort(sapply(l$layout$annotations, "[[", "x")),
    c(1.835, 5.250)
  )
  expect_equivalent(
    sort(sapply(l$layout$annotations, "[[", "y")),
    c(10.4, 33.9)
  )
})

