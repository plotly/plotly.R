library(plotly)

storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

subplot(
  nrows = 2, 
  plot_geo(storms, name = "2D storms"),
  plot_geo(storms, name = "3D storms") %>% 
    layout(
      geo = list(
        projection = list(type = "orthographic"),
        lonaxis = list(showgrid = TRUE, gridcolor = "gray"),
        lataxis = list(showgrid = TRUE, gridcolor = "gray")
      )
    )
)

