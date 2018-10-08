library(plotly)
# devtools::install_github("rstudio/leaflet#346")
library(leaflet)
library(crosstalk)
library(htmltools)

# leaflet should respect these "global" highlight() options
options(opacityDim = 0.5)

sd <- highlight_key(quakes)

p <- plot_ly(sd, x = ~depth, y = ~mag) %>% 
  add_markers(alpha = 0.5) %>%
  highlight("plotly_selected", dynamic = TRUE)

map <- leaflet(sd) %>% 
  addTiles() %>% 
  addCircles()

bscols(p, map)
