library(plotly)
# devtools::install_github("rstudio/leaflet#281")
library(leaflet)
library(crosstalk)
library(htmltools)

# leaflet should respect these "global" highlight() options
options(opacityDim = 0.01, persist = TRUE)

sd <- SharedData$new(quakes)
p <- plot_ly(sd, x = ~depth, y = ~mag) %>% add_markers(alpha = 0.5)
map <- leaflet(sd) %>% addTiles() %>% addCircles()
browsable(tagList(list(p, map)))
