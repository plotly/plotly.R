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
plot_mapbox(nc, text = ~AREA, hoverinfo = "text")

# each trace may have at most one fill color, so if you'd like to
# create choropleths, for instance, you should `split`  
col_scale <- scales::col_numeric("Blues", range(nc$AREA))
plot_mapbox(nc, split = ~factor(AREA), fillcolor = ~col_scale(AREA)) %>%
  layout(showlegend = FALSE)

# TODO: 
# (1) this should create multiple traces!!! :(
# (2) perhaps the colorbar definition needs fixing?
plot_mapbox(nc, color = ~AREA, text = ~AREA, hoverinfo = "text", hoveron = "fill") %>%
  plotly_json()

# TODO: animation


# TODO: click to highlight
ncsd <- crosstalk::SharedData$new(nc)
plot_mapbox(ncsd)


# TODO: perhaps during verification, if hoveron = 'fill' for a given trace,
# we could check if text is unique or not...if it is, just take first element
plot_mapbox(nc, split = ~AREA, text = ~NAME, hoveron = "fill")

# TODO: how to best control hoverinfo?

# add dropdown for changing baselayer
# TODO: how to keep the bounding box fixed?
styles <- schema(FALSE)$layout$layoutAttributes$mapbox$style$values
style_buttons <- lapply(styles, function(s) {
  list(label = s, method = "relayout", args = list("mapbox.style", s))
})

p1 %>%
  layout(
    title = "Changing the base layer",
    updatemenus = list(list(y = 0.8, buttons = style_buttons))
  )


# non-standard crs
library(mapview)
plot_mapbox(trails)

library(crosstalk)
tsd <- SharedData$new(trails)

bscols(
  plot_mapbox(tsd, text = ~district, hoverinfo = "text"),
  DT::datatable(tsd)
)



