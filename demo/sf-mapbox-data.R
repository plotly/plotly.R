library(sf)
library(plotly)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# can set multiple bounding boxes and overwrite attributes 
subplot(
  plot_mapbox(nc),
  plot_mapbox(nc, fillcolor = "gray", line = list(size = 0.01, color = "black"))
)

# can map custom hover text to each point
plot_mapbox(nc, text = ~AREA, hoverinfo = "text")

# TODO: 
# (1) this should create multiple traces!!! :(
# (2) perhaps the colorbar definition needs fixing?
plot_mapbox(nc, color = ~AREA, text = ~AREA, hoverinfo = "text", hoveron = "fill") %>%
  plotly_json()

# TODO: animation



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


