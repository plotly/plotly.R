# These examples demonstrate ways to display binned/aggregated selections
library(crosstalk)
library(plotly)

d <- SharedData$new(mtcars)
scatterplot <- plot_ly(d, x = ~mpg, y = ~disp) %>%
  add_markers(color = I("black")) %>%
  layout(dragmode = "select")

# add_histogram() does both continuous _and_ discrete binning in the browser,
# allowing us to perform aggregations on the fly, without 
p <- subplot(
  plot_ly(d, x = ~factor(cyl)) %>% add_histogram(color = I("black")),
  scatterplot
) 

# Crosstalk selections are actually additional traces, and, by default, 
# plotly.js will try to dodge bars placed under the same category
layout(p, barmode = "overlay")

subplot(
  plot_ly(d, y = ~disp, color = I("black")) %>% add_boxplot(name = "overall"),
  scatterplot,
  shareY = TRUE
) %>% layout(dragmode = "select")

