library(plotly)
library(dplyr)

# latitude, longitude, and altitiude of tropical storms
storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

# even grid of lat/lons spanning the globe (for creating the globe surface)
nlat <- 200
nlon <- 100
lat <- seq(-180, 180, length.out = nlat)
lon <- seq(-90, 90, length.out = nlon)
lat <- matrix(rep(lat, nlon), nrow = nlat)
lon <- matrix(rep(lon, each = nlat), nrow = nlat)

# helper function for converting polar (lat/lon) -> cartesian (x/y/z)
degrees2radians <- function(degree) degree * pi / 180 

# show as little as possible when hovering over surface
empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE,
  showspikes = FALSE,
  spikesides = FALSE,
  title = ""
)

# for centering camera/lighting on the center of the storm paths
xyzmean <- list(x = .41, y = -.71, z = 0.57)


# A 3D globe implemented with 3D lines and a spherical surface
# Note that the globe has a radius of 1, but project the lines with 
# a radius of 1.001 so that we appear on top of the surface
globe <- plot_ly(height = 500) %>%
  add_sf(
    data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)), 
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = I("black"), size = I(1),
    hoverinfo = "none"
  ) %>%
  add_sf(
    data = highlight_key(storms, group = "storm paths"),
    name = "storm paths",
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = ~z, size = I(6),
    text = ~paste("Latitude:", y, "<br>", "Longitude:", x, "<br>", "Altitude:", z),
    hoverinfo = "text"
  ) %>%
  add_surface(
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
    z = sin(degrees2radians(lat)),
    # NOTE: you can map a value to surfacecolor to encode, say air temp
    # for an example, see https://github.com/cpsievert/Weather_Stuff/blob/master/radiation-plot-3D.R
    # But here is a trick to set the surface color to a constant white
    surfacecolor = matrix(NA, nrow = nlat, ncol = nlon),
    showscale = FALSE, hoverinfo = "skip",
    lightposition = xyzmean, 
    contours = list(
      x = list(highlight = FALSE), 
      y = list(highlight = FALSE), 
      z = list(highlight = FALSE)
    )
  ) %>%
  layout(
    showlegend = FALSE,
    scene = list(
      xaxis = empty_axis,
      yaxis = empty_axis,
      zaxis = empty_axis,
      aspectratio = list(x = 1, y = 1, z = 1),
      camera = list(eye = xyzmean)
    )
  )


# spherical distance between the first point and every other point
# https://en.wikipedia.org/wiki/Great-circle_distance
arc_dist <- function(lon, lat) {
  lon <- degrees2radians(lon)
  lat <- degrees2radians(lat)
  lon0 <- lon[1]
  lat0 <- lat[1]
  delta <- cos(abs(lon - lon0))
  acos(sin(lat0) * sin(lat) + cos(lat0) * cos(lat) * delta)
}

# plot altitude of each storm versus the distance it has traveled
distanceByAlt <- storms %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  group_by(L1) %>%
  mutate(dist = arc_dist(X, Y)) %>%
  rename(altitude = Z) %>%
  highlight_key(~L1, group = "storm paths") %>%
  plot_ly(x = ~dist, y = ~altitude, height = 400) %>%
  # plotly.js doesn't support color gradient along *2D* lines
  add_lines(color = I("gray")) %>%
  add_markers(
    color = ~altitude, hoverinfo = "text",
    text = ~paste("Distance:", round(dist, 2), "<br>", "Altitude:", altitude, "<br>", "Storm:", L1)
  ) %>%
  layout(
    showlegend = FALSE,
    title = "Tropical storm altitude by distance \n (click to highlight storm)",
    font = list(size = 15, family = "Balta"),
    margin = list(t = 60)
  )

# force persistent selection
# TODO: persistence via shift should work with two separate graphs!!
options(persistent = TRUE)

library(htmltools)
browsable(tagList(globe, distanceByAlt))
