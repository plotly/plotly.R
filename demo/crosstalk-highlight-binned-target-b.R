# These examples demonstrate ways to display binned/aggregated selections
library(plotly)

d <- highlight_key(mtcars)
sp <- plot_ly(d, x = ~mpg, y = ~disp) %>%
  add_markers(color = I("black"))

# 'statistical trace types'
hist <- plot_ly(d, x = ~factor(cyl)) %>% 
  add_histogram(color = I("black"))
box <- plot_ly(d, y = ~disp, color = I("black")) %>% 
  add_boxplot(name = " ")
violin <- plot_ly(d, y = ~disp, color = I("black")) %>%
  add_trace(type = "violin", name = " ")

subplot(sp, box, violin, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
  subplot(hist, widths = c(.75, .25), titleX = TRUE, titleY = TRUE) %>%
  layout(
    barmode = "overlay", 
    title = "Click and drag scatterplot",
    showlegend = FALSE
  ) %>%
  highlight("plotly_selected")
