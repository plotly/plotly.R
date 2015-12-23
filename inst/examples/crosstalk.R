# access renderPlotly() selections on a shiny server
# https://github.com/cpsievert/shiny_apps/blob/master/plotlyCrosstalk/app.R

# TODO: 
# (1) pass selections from another htmlwidget (rcdimple?) to renderPlotly()
# (2) crosstalk without shiny (see examples below)
# (3) define custom event behavior in JS from R?

library(plotly)
library(htmltools)

# click/show selects?
mtcars$gear <- factor(mtcars$gear)
mtcars$cyl <- factor(mtcars$cyl)
p1 <- plot_ly(mtcars, x = wt, y = mpg, color = gear, mode = "markers", 
              key = gear, set = "A", width = 400)
p2 <- plot_ly(mtcars, x = wt, y = disp, color = gear, mode = "markers", 
              key = gear, set = "A", width = 400)
# TODO: inline-block?
browsable(tagList(
  as.widget(p1),
  as.widget(p2)
))

library(dplyr)
m <- count(mtcars, cyl)
p1 <- plot_ly(m, x = cyl, y = n, type = "bar", 
              key = cyl, set = "A", width = 400)
p2 <- plot_ly(mtcars, x = mpg, y = disp, mode = "markers", 
              key = cyl, set = "A", width = 400)
browsable(tagList(
  as.widget(p1),
  as.widget(p2)
))
