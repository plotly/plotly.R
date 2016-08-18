library(plotly)
library(crosstalk)

sd <- SharedData$new(txhousing, ~city)

base <- plot_ly(sd, color = I("black")) %>%
  group_by(city)

library(plotly)
library(crosstalk)

sd <- SharedData$new(txhousing, ~city)

base <- plot_ly(sd, color = I("black")) %>%
  group_by(city)

p1 <- base %>%
  summarise(has = sum(is.na(median))) %>%
  filter(has > 0) %>%
  arrange(has) %>%
  add_bars(x = ~has, y = ~factor(city, levels = city), orientation = "h") %>%
  layout(
    barmode = "overlay",
    xaxis = list(title = "Number of months missing"),
    yaxis = list(title = "")
  ) 

p2 <- base %>%
  add_lines(x = ~date, y = ~median, alpha = 0.3) %>%
  layout(xaxis = list(title = ""))

p <- subplot(p1, p2, titleX = TRUE) %>% layout(margin = list(l = 200)) 

# select individual series on click
crosstalk(p, on = "plotly_click", off = "plotly_unhover", color = "red")

# compare multiple series
crosstalk(
  p, on = "plotly_click", off = "plotly_doubleclick", 
  dynamic = TRUE, persistent = TRUE
)
