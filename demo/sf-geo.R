library(sf)
library(plotly)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
storms <- st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

# TODO: geometry should be added as a grouping variable?
subplot(
  plot_geo(nc),
  plot_geo(storms)
)

