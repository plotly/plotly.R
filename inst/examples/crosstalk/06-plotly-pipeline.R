library(plotly)
library(crosstalk)

sd <- SharedData$new(txhousing, ~city)

base <- plot_ly(sd, color = I("black")) %>%
  group_by(city) %>%
  crosstalk(on = "plotly_click", off = "plotly_unhover", color = "red")

p1 <- base %>%
  summarise(has = sum(is.na(median))) %>%
  filter(has > 0) %>%
  arrange(has) %>%
  add_bars(x = ~has, y = ~city, orientation = "h") %>%
  layout(
    barmode = "overlay", 
    xaxis = list(title = "Missing months"),
    yaxis = list(title = ""),
    margin = list(l = 120)
  ) 

p2 <- base %>%
  add_lines(x = ~date, y = ~median, alpha = 0.3)

subplot(p1, p2)
