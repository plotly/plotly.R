library(plotly)
library(leaflet)
library(crosstalk)
library(htmltools)

sd <- SharedData$new(quakes)
p <- plot_ly(sd, x = ~depth, y = ~mag) %>% add_markers(alpha = 0.5)
map <- leaflet(sd) %>% addTiles() %>% addCircles()
browsable(tagList(list(p, map)))
