library(plotly)
library(crosstalk)

d <- SharedData$new(txhousing, ~city)
p <- qplot(data = d, x = date, y = median, group = city, geom = "line")
ggplotly(p, tooltip = "city") %>%
  crosstalk(on = "plotly_hover", color = "red")
