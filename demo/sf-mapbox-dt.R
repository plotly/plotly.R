library(plotly)
library(crosstalk)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
ncsd <- SharedData$new(nc)

map <- plot_mapbox(ncsd, text = ~CNTY_ID, hoverinfo = "text") %>%
  highlight(persistent = TRUE) %>%
  layout(title = "Click on counties to query them") %>%
  config(displayModeBar = FALSE)

bscols(map, DT::datatable(ncsd))
