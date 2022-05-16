context("rangeslider")

p1 <- plot_ly(x = time(USAccDeaths), y = USAccDeaths) %>% 
  add_lines() %>%
  rangeslider()

d <- tibble::tibble(
  time = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by = "days"),
  y = rnorm(seq_along(time))
)

p2 <- plot_ly(d, x = ~time, y = ~y) %>%
  add_lines() %>%
  rangeslider(d$time[5], d$time[50])

p3 <- subplot(p1, p2, nrows = 2, margin = 0.1) 

test_that("Basic rangeslider", {
  expect_doppelganger(p1, "rangeslider")
})

test_that("Rangeslider range", {
  expect_doppelganger(p2, "rangeslider-range")
})

test_that("Rangeslider subplot", {
  expect_doppelganger(p3, "rangeslider-subplot")
})

test_that("Rangeslider multiple axes", {
  p <- subplot(qplot(1:10), qplot(1:10, 1:10)) %>%
    rangeslider()
  expect_doppelganger(p, "rangeslider-multiple-axes")
})

test_that("Rangeslider targetted axis", {
  p <- subplot(qplot(1:10), qplot(1:10, 1:10)) %>%
    rangeslider(xaxes = "xaxis2")
  expect_doppelganger(p, "rangeslider-targetted-axis")
})
