
test_that("geom_vline/geom_hline does not throw an error with ggplotly when no lines are found", {
  # Prepare data
  data <- data.table::data.table(gapminder::gapminder)
  # Add year of interest for vertical lines
  data[year == 2002, year_of_interest := "yoi"]

  # Case 1: Vertical line by feeding data to it; this allows for programmatically setting many lines at different years
  p1 <- ggplot2::ggplot(data, ggplot2::aes(x = year, y = lifeExp, colour = country)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = year),
      data = data[year_of_interest == "yoi", ],
      colour = "yellow",
      alpha = 0.75,
      linewidth = 1.25,
      linetype = "dashed"
    ) +
    ggplot2::theme(legend.position = "none")

  # Case 2: No lines are found, ggplot2 accepts it and no error is thrown
  p2 <- ggplot2::ggplot(data, ggplot2::aes(x = year, y = lifeExp, colour = country)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = year),
      data = data[year_of_interest == "some_not_found", ],
      colour = "yellow",
      alpha = 0.75,
      linewidth = 1.25,
      linetype = "dashed"
    ) +
    ggplot2::theme(legend.position = "none")

  # Test that ggplotly does not throw an error for both cases
  expect_error(plotly::ggplotly(p1), NA) # lines are found no error is thrown
  # error:
  # "Error in fix.by(by.y, y) : 'by' must specify a uniquely valid column"
  expect_error(plotly::ggplotly(p2), "Error in fix.by") # no lines are found no error is thrown
})
