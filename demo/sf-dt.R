library(plotly)
library(crosstalk)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
ncsd <- SharedData$new(nc)

map <- plot_ly(ncsd, split = ~NAME, color = I("gray"), hoveron = "fills") %>%
  highlight(persistent = TRUE, color = "red", opacityDim = 1) %>%
  layout(title = "Click on counties to query them", showlegend = FALSE) %>%
  config(displayModeBar = FALSE)

bscols(map, DT::datatable(ncsd))
