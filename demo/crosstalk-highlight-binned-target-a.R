# These examples demonstrate ways to display binned/aggregated selections
library(crosstalk)
library(plotly)

d <- SharedData$new(mpg)
dots <- plot_ly(d, color = ~class, x = ~displ, y = ~cyl)
boxs <- plot_ly(d, color = ~class, x = ~class, y = ~cty) %>% add_boxplot()
bars <- plot_ly(d, x = ~class, color = ~class)

subplot(dots, boxs, titleX = TRUE, titleY = TRUE) %>%
  subplot(bars, nrows = 2, titleX = TRUE, titleY = TRUE) %>%
  layout(
    title = "Click and drag on scatterplot",
    barmode = "overlay",
    showlegend = FALSE
  ) %>%
  highlight("plotly_selected")
