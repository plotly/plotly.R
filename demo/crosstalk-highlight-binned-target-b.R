# These examples demonstrate ways to display binned/aggregated selections
library(crosstalk)
library(plotly)

d <- SharedData$new(mtcars)
sp <- plot_ly(d, x = ~mpg, y = ~disp) %>%
  add_markers(color = I("black"))

# hist/box/violin are all 'statistical trace types' meaning
# they compute aggregations on the fly
hist <- plot_ly(d, x = ~factor(cyl)) %>% 
  add_histogram(color = I("black"))
box <- plot_ly(d, y = ~disp, color = I("black")) %>% 
  add_boxplot(name = "overall")
violin <- plot_ly(d, y = ~disp, color = I("black")) %>%
  add_trace(type = "violin", name = "overall")

subplot(sp, hist, box, violin, nrows = 1) %>%
  layout(
    barmode = "overlay", 
    title = "Click and drag scatterplot"
  ) %>%
  highlight(
    "plotly_selected",
    selected = attrs_selected(showlegend = FALSE)
  )
