# These examples demonstrate ways to display binned/aggregated selections
library(crosstalk)
library(plotly)

d <- SharedData$new(mtcars)
scatterplot <- plot_ly(d, x = ~mpg, y = ~disp) %>%
  add_markers(color = I("black")) %>%
  highlight("plotly_selected")

# add_histogram() does both continuous _and_ discrete binning in the browser,
# allowing us to perform aggregations on the fly, without 
p <- subplot(
  plot_ly(d, x = ~factor(cyl)) %>% add_histogram(color = I("black")),
  scatterplot
) 

# Crosstalk selections are actually additional traces, and, by default, 
# plotly.js will try to dodge bars placed under the same category
layout(p, barmode = "overlay") %>%
  highlight(
    "plotly_selected",
    selected = attrs_selected(showlegend = FALSE)
  )

# same idea, but now with a boxplot
p <- plot_ly(d, y = ~disp, color = I("black")) %>% add_boxplot(name = "overall")
subplot(p, scatterplot, shareY = TRUE) %>% 
  highlight(
    "plotly_selected",
    selected = attrs_selected(name = "selection")
  )



tx <- SharedData$new(txhousing, ~city)
p1 <- ggplot(tx, aes(date, median, group = city)) + geom_line() + xlab(NULL)
gg1 <- ggplotly(p1, tooltip = c("city", "date", "median"))
p2 <- plot_ly(tx, x = ~median, color = I("black")) %>% 
  add_histogram(histnorm = "probability density")

subplot(gg1, p2, titleX = TRUE, titleY = TRUE) %>% 
  layout(barmode = "overlay") %>%
  highlight(
    dynamic = TRUE, persistent = TRUE, 
    selected = attrs_selected(opacity = 0.3)
  )




d <- SharedData$new(mpg)
dots <- plot_ly(d, color = ~class, x = ~displ, y = ~cyl)
boxs <- plot_ly(d, color = ~class, x = ~class, y = ~cty) %>% add_boxplot()
bars <- plot_ly(d, x = ~class, color = ~class)

subplot(dots, boxs) %>%
  subplot(bars, nrows = 2) %>%
  layout(
    dragmode = "select",
    barmode = "overlay",
    showlegend = FALSE
  ) %>%
  highlight("plotly_selected")


