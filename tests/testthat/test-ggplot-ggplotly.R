context("ggplotly+plotly")

p <- ggplot(txhousing, aes(x = date, y = median, group = city)) +
  geom_line(alpha = 0.3)

test_that("ggplotly returns original data with special attributes", {
  dat <- ggplotly(p) %>% plotly_data()
  expect_equal(dat, p$data)
  expect_equal(as.character(dplyr::groups(dat)), "city")
})

test_that("can filter data returned by ggplotly", {
  dat <- ggplotly(p) %>% filter(city == "Houston") %>% plotly_data()
  expect_equal(dat, subset(p$data, city == "Houston"))
  expect_equal(as.character(dplyr::groups(dat)), "city")
})

test_that("can add traces with original _and_ scaled data", {
  l1 <- ggplotly(p) %>% add_lines() %>% plotly_build() %>% .[["x"]]
  expect_equal(length(l$data), 2)
  l2 <- ggplotly(p, originalData = FALSE) %>% 
    add_lines() %>% plotly_build() %>% .[["x"]]
  expect_equal(l1, l2)
})

test_that("can access ggplot data in layout()", {
  l <- ggplotly(p) %>% layout(title = ~range(date))
  expect_equal(plotly_build(l)$x$layout$title, range(txhousing$date))
})
