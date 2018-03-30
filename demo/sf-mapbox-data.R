library(sf)

plot_mapbox(res_mn)
plot_mapbox(res_mn, split = ~INDRESNAME)


library(sf)
library(plotly)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# can set multiple bounding boxes and overwrite attributes 
subplot(
  plot_mapbox(nc),
  plot_mapbox(nc, fillcolor = "gray", line = list(size = 0.01, color = "black"))
)

# map custom hover text to each point
# (unfortunately, scattermapbox does not yet support hoveron='fill')
plot_mapbox(nc, text = ~paste0(NAME, ": ", AREA), hoverinfo = "text")


col_scale <- scales::col_numeric("Blues", range(nc$AREA))
plot_ly(nc, fillcolor = ~col_scale(AREA), text = ~AREA, hoveron = "fill") %>%
  layout(showlegend = FALSE)

# TODO: animation


# TODO: click to highlight
ncsd <- crosstalk::SharedData$new(nc)
plot_mapbox(ncsd)


# TODO: perhaps during verification, if hoveron = 'fill' for a given trace,
# we could check if text is unique or not...if it is, just take first element
plot_mapbox(nc, split = ~AREA, text = ~NAME, hoveron = "fill")

# TODO: how to best control hoverinfo?



# non-standard crs
library(mapview)
plot_mapbox(trails)



