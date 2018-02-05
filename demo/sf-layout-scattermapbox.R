library(sf)
library(plotly)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# TODO: is there a way to do this without writing to disk?
sf_to_geojson <- function(x) {
  tmp <- tempfile(fileext = ".geojson")
  st_write(x, tmp, driver = "GEOJSON")
  geojsonio::geojson_read(tmp, "local")
}

# By converting sf to geojson and routing to mapbox.layers, rendering 
# should be more performant (and correct in all cases). However, compared to
# `demo("sf-data-scattermapbox.R", package = "plotly")`, you lose the ability 
# to interact with (and link) the features.
plot_mapbox(x = -80, y = 35) %>%
  layout(
    hovermode = 'closest',
    mapbox = list(
      layers = list(
        list(
          sourcetype = 'geojson',
          source = sf_to_geojson(nc),
          type = 'fill',
          color = 'transparent'
        )
      ),
      bearing = 0,
      center = list(lat = 35, lon = -80),
      pitch = 0,
      zoom = 5.2,
      style = 'light'
    )
  )
