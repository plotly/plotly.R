library(albersusa)
library(sf)
library(dplyr)
library(plotly)

d <- counties_sf() %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  filter(!state %in% c("Alaska", "Hawaii"))

# TODO: is there a way to do this without writing to disk?
sf_to_geojson <- function(x) {
  tmp <- tempfile(fileext = ".geojson")
  st_write(x, tmp, driver = "GEOJSON")
  geojsonio::geojson_read(tmp, "local")
}

plot_mapbox(lat = 45.5017, lon = -73.5673) %>%
  layout(
    height = 600,
    autosize = TRUE,
    hovermode = 'closest',
    mapbox = list(
      layers = list(
        list(
          sourcetype = 'geojson',
          source = sf_to_geojson(d),
          type = 'fill',
          color = 'transparent'
        )
      ),
      bearing = 0,
      center = list(lat = 27.8, lon = -83),
      pitch = 0,
      zoom = 5.2,
      style = 'light'
    )
  )
