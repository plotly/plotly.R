library(sf)

# https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2017&layergroup=American+Indian+Area+Geography
# click 'American Indian/Alaska Native/Native Hawaiian Area'
res <- sf::st_read("~/Downloads/tl_2017_us_aiannh/tl_2017_us_aiannh.shp")

# TODO: 
# (1) simplify shape file!!
# (2) crosstalk highlight should set fillcolor...

res %>%
  highlight_unit(~NAME) %>%
  plot_ly(text = ~NAME) %>%
  highlight(selectize = TRUE, dynamic = TRUE)

