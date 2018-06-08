library(GGally)
d <- highlight_unit(iris)
p <- ggpairs(d, aes(colour = Species), columns = 1:5)
ggplotly(p) %>% 
  highlight("plotly_selected")
