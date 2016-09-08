library(plotly)
library(crosstalk)

d <- SharedData$new(txhousing, ~city)
p <- ggplot(d, aes(date, median, group = city)) +
  geom_line(geom = "line")
ggplotly(p, tooltip = "city") %>%
  highlight(on = "plotly_hover", color = "red")
