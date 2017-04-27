library(GGally)
d <- SharedData$new(iris)
p <- GGally::ggpairs(d, aes(colour = Species), columns = 1:5)
ggplotly(p) %>% highlight("plotly_selected")
