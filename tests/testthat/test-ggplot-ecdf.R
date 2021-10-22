test_that("`stat_ecdf` renders correctly", {
  df <- data.frame(
    x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
    g = gl(2, 100)
  )

  p <- ggplot(df, aes(x)) +
      stat_ecdf(geom = "step")

  expect_doppelganger(ggplotly(p), "step-ecdf")
})