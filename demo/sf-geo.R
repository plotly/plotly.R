library(sf)
library(plotly)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
storms <- st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

subplot(
  nrows = 2, 
  plot_geo(nc, name = "NC counties"),
  plot_geo(storms, name = "Storms") %>% 
    layout(
      geo = list(
        projection = list(type = "orthographic"),
        lonaxis = list(showgrid = TRUE, gridcolor = "gray"),
        lataxis = list(showgrid = TRUE, gridcolor = "gray")
      )
    )
)

