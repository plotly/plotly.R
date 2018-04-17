library(plotly)

storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

# obtain all the "pre-packaged" mapbox styles and use them to 
# create a collection of buttons that will control the mapbox.style 
# attribute via a dropdown -- https://plot.ly/r/dropdowns/
styles <- schema(FALSE)$layout$layoutAttributes$mapbox$style$values
style_buttons <- lapply(styles, function(s) {
  list(label = s, method = "relayout", args = list("mapbox.style", s))
})

plot_mapbox(storms, color = I("red")) %>%
  layout(
    title = "Changing the base layer",
    updatemenus = list(list(y = 0.8, buttons = style_buttons))
  )

