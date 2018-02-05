library(sf)
library(plotly)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
bb <- st_bbox(nc)

# TODO: make this more obvious and automatic
plot_mapbox(
  data = group2NA(nc), 
  x = ~x, 
  y = ~y, 
  mode = "lines", 
  fillcolor = "green", 
  fill = "toself", 
  line = list(size = 0.01, color = "black")
) %>%
  layout(
    mapbox = list(
      center = list(
        lat  = mean(bb[c("ymin", "ymax")]), 
        lon = mean(bb[c("xmin", "xmax")])
      ),
      zoom = 6
    )
  )
