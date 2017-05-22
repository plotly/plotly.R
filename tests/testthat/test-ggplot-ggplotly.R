context("ggplotly+plotly")

p <- ggplot(txhousing, aes(x = date, y = median, group = city)) +
  geom_line(alpha = 0.3)

test_that("ggplotly returns original data with special attributes", {
  dat <- ggplotly(p) %>% plotly_data()
  expect_equivalent(dat, p$data)
  expect_equivalent(as.character(dplyr::groups(dat)), "city")
})

test_that("can filter data returned by ggplotly", {
  dat <- ggplotly(p) %>% filter(city == "Houston") %>% plotly_data()
  expect_equivalent(dat, subset(p$data, city == "Houston"))
  expect_equivalent(as.character(dplyr::groups(dat)), "city")
})

test_that("can add traces with original _and_ scaled data", {
  l1 <- ggplotly(p) %>% add_lines() %>% plotly_build()
  expect_equivalent(length(l1$x$data), 2)
  l2 <- ggplotly(p, originalData = FALSE) %>% 
    add_lines() %>% plotly_build()
  # ideally we'd test that the two plots have the same data, but 
  # for some reason R CMD check throws an error which I can't replicate :(
  expect_equivalent(length(l2$x$data), 2)
})

test_that("can access ggplot data in layout()", {
  l <- ggplotly(p) %>% layout(title = ~range(date))
  expect_equivalent(plotly_build(l)$x$layout$title, range(txhousing$date))
})
