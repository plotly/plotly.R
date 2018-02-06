library(sf)
library(plotly)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

p1 <- plot_mapbox(
  nc,
  mode = "lines", 
  fillcolor = "green", 
  fill = "toself", 
  line = list(size = 0.01, color = "black")
)

subplot(p1, p1)

# TODO: dropdown button for changing the baselayer
