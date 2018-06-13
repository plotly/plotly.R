# These examples demonstrate ways to display binned/aggregated selections
library(plotly)

tx <- highlight_key(txhousing, ~city)
p1 <- ggplot(tx, aes(date, median, group = city)) + geom_line() + xlab(NULL)
gg1 <- ggplotly(p1, tooltip = c("city", "date", "median"))
p2 <- plot_ly(tx, x = ~median, color = I("black")) %>% 
  add_histogram(histnorm = "probability density")

subplot(gg1, p2, titleX = TRUE, titleY = TRUE) %>% 
  layout(barmode = "overlay") %>%
  highlight(dynamic = TRUE, selected = attrs_selected(opacity = 0.3))
