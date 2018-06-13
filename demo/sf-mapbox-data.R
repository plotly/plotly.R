library(plotly)
library(crosstalk)

# one trace
plot_mapbox(res_mn)
plot_mapbox(res_mn, stroke = I("#119dff"), span = I(1), color = I("#00cc96"))

# multiple traces 
plot_mapbox(res_mn, split = ~INDRESNAME, span = I(1))
plot_mapbox(res_mn, split = ~INDRESNAME, color = ~AREA, stroke = ~PERIMETER, span = I(1))

# linking with DT
mn <- highlight_key(res_mn)
bscols(
  plot_mapbox(mn, split = ~INDRESNAME, text = ~INDRESNAME, hoverinfo = "text", hoveron = "fills") %>%
    layout(title = "Click a reservation", showlegend = FALSE),
  DT::datatable(mn)
)

# linking with plotly
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
ncsd <- highlight_key(nc)

# note that brushing counties is currently possible with plot_ly(), but isn't quite working 
# yet with plot_mapbox() -- https://github.com/plotly/plotly.js/issues/2512
bscols(
  plot_mapbox(ncsd) %>%
    highlight(dynamic = TRUE, persistent = TRUE),
  plot_ly(ncsd, x = ~AREA) %>% 
    add_histogram(xbins = list(start = 0, end = 0.3, size = 0.02)) %>%
    layout(barmode = "overlay") %>% 
    highlight("plotly_selected", persistent = TRUE)
)


