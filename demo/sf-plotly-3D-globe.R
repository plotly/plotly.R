library(plotly)

storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE,
  title = ""
)

# even grid of lat/lons along the whole globe
nlat <- 200
nlon <- 100
lat <- seq(-180, 180, length.out = nlat)
lon <- seq(-90, 90, length.out = nlon)
lat <- matrix(rep(lat, nlon), nrow = nlat)
lon <- matrix(rep(lon, each = nlat), nrow = nlat)

# helper function for converting polar -> cartesian
degrees2radians <- function(degree) degree * pi / 180 

plot_ly() %>%
  add_sf(
    data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)), 
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = I("black"), hoverinfo = "none", size = I(1),
    showlegend = FALSE
  ) %>%
  add_sf(
    data = storms,
    name = "storm paths",
    x = ~ cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ sin(degrees2radians(x)) * cos(degrees2radians(y)),
    # TODO: this is just a guess, the rescaling range is not actually [0, 0.2]...
    z = ~ sin(degrees2radians(y)) + scales::rescale(z, to = c(0, 0.2))
  ) %>%
  add_surface(
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
    z = sin(degrees2radians(lat)),
    # TODO: perhaps there is a better way to specify a constant surfacecolor?
    surfacecolor = matrix(rep(0.5, nlat * nlon), nrow = nlat),
    colorscale = data.frame(c(0, 0.5, 1), c("red", "white", "blue")),
    showscale = FALSE,
    hoverinfo = "none"
  ) %>%
  # TODO: is there a way to turn on the spikelines?
  layout(
    scene = list(
      xaxis = empty_axis,
      yaxis = empty_axis,
      zaxis = empty_axis,
      aspectratio = list(x = 1, y = 1, z = 1),
      camera = list(eye = list(x = .41, y = -.71, z = 0.57))
    )
  )
  
