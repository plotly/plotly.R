library(plotly)
library(crosstalk)

sd <- SharedData$new(txhousing, ~city, "Select a city")

base <- plot_ly(sd, color = I("black")) %>%
  group_by(city)

p1 <- base %>%
  summarise(has = sum(is.na(median))) %>%
  filter(has > 0) %>%
  arrange(has) %>%
  add_bars(x = ~has, y = ~factor(city, levels = city), hoverinfo = "x+y") %>%
  layout(
    barmode = "overlay",
    xaxis = list(title = "Number of months missing"),
    yaxis = list(title = "")
  ) 

p2 <- base %>%
  add_lines(x = ~date, y = ~median, alpha = 0.3) %>%
  layout(xaxis = list(title = ""))

 subplot(p1, p2, titleX = TRUE, widths = c(0.3, 0.7)) %>% 
  layout(margin = list(l = 120)) %>%
  hide_legend() %>%
  highlight(dynamic = TRUE, persistent = TRUE, selectize = TRUE)
