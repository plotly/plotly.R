
test_that("geom_vline/geom_hline does not throw an error with ggplotly when no lines are found", {
  set.seed(123)
  x <- seq(0, 10, by = 1)
  # random walk
  y <- cumsum(rnorm(length(x), mean = 0, sd = 1))
  df <- data.frame(x, y)
  df$point_of_interest <- "not_poi"
  df[df$x == 2, ]$point_of_interest <- "poi" # point of interest

  # Case 1: Vertical line by feeding data to it; this allows for programmatically setting many lines at different years
  p1 <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = x),
      data = df[df$point_of_interest == "poi", ],
      colour = "yellow"
    )

  # Test that ggplotly does not throw an error for both cases
  expect_error(plotly::ggplotly(p1), NA) # lines are found no error is thrown

  # Case 2: No lines are found, ggplot2 accepts it and no error is thrown
  p2 <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = x),
      data = df[df$point_of_interest == "something_not_matched", ],
      colour = "yellow"
    )

  # error given without fix:
  # "Error in fix.by(by.y, y) : 'by' must specify a uniquely valid column"
  expect_error(plotly::ggplotly(p2), NA) # no lines are found no error is thrown

  ## --------------------------------
  # testing horizontal line
  df$value_of_interest <- "not_voi"
  df[round(df$x, 0) == 2, ]$value_of_interest <- "voi" # value of interest

  # horizontal line no error
  p3 <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = x),
      data = df[df$value_of_interest == "voi", ],
      colour = "pink"
    )

  expect_error(plotly::ggplotly(p3), NA) # lines are found no error is thrown

  # horizontal line not set; error
  p4 <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = x),
      data = df[df$value_of_interest == "something_not_matched", ],
      colour = "pink"
    )

  # error given without fix:
  # "Error in fix.by(by.y, y) : 'by' must specify a uniquely valid column"
  expect_error(plotly::ggplotly(p4), NA) # no lines are found no error is thrown
})
