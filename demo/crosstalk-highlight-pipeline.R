library(plotly)

sd <- highlight_key(txhousing, ~city, "Select a city")

base <- plot_ly(sd, color = I("black"), height = 400) %>%
  group_by(city)

p1 <- base %>%
  summarise(miss = sum(is.na(median))) %>%
  filter(miss > 0) %>%
  add_markers(x = ~miss, y = ~forcats::fct_reorder(city, miss), hoverinfo = "x+y") %>%
  layout(
    barmode = "overlay",
    xaxis = list(title = "Number of months missing"),
    yaxis = list(title = "")
  ) 

p2 <- base %>%
  add_lines(x = ~date, y = ~median, alpha = 0.3) %>%
  layout(xaxis = list(title = ""))

 subplot(p1, p2, titleX = TRUE, widths = c(0.3, 0.7)) %>% 
  hide_legend() %>%
  highlight(dynamic = TRUE, selectize = TRUE)
