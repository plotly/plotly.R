library(shiny)
library(plotly)

data(movies, package = "ggplot2")
minx <- min(movies$rating)
maxx <- max(movies$rating)

shinyServer(function(input, output) {
  
  output$trendPlot <- renderPlotly({
    # size of the bins depend on the input 'bins'
    size <- (maxx - minx) / input$bins
    
    # a simple histogram of movie ratings
    p <- plot_ly(movies, x = rating, autobinx = F, type = "histogram",
                 xbins = list(start = minx, end = maxx, size = size))
    # style the xaxis
    layout(p, xaxis = list(title = "Ratings", range = c(minx, maxx), autorange = F,
                           autotick = F, tick0 = minx, dtick = size))
  })
})

