# These examples demonstrate ways to display binned/aggregated selections
library(plotly)

d <- highlight_key(mpg)
dots <- plot_ly(d, colors = "Set1", color = ~class, x = ~displ, y = ~jitter(cyl)) %>%
  layout(
    xaxis = list(title = "Engine displacement"),
    yaxis = list(title = "Number of cylinders")
  )
boxs <- plot_ly(d, colors = "Set1", color = ~class, x = ~class, y = ~cty) %>% 
  add_boxplot() %>%
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Miles per gallon (city)")
  )
bars <- plot_ly(d, colors = "Set1", x = ~class, color = ~class)

subplot(dots, boxs, titleX = TRUE, titleY = TRUE) %>%
  subplot(bars, nrows = 2, titleX = TRUE, titleY = TRUE) %>%
  layout(
    title = "Dynamic 2-way ANOVA (click & drag on scatterplot)",
    barmode = "overlay",
    showlegend = FALSE
  ) %>%
  highlight("plotly_selected", opacityDim = 0.6)
