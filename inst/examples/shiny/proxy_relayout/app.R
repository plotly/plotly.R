library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot")
)

server <- function(input, output, session) {
  
  p <- ggplot(txhousing) +
    geom_line(aes(date, median, group = city))
  
  output$plot <- renderPlotly({
    ggplotly(p, dynamicTicks = TRUE) %>% 
      rangeslider() 
  })
  
  observeEvent(event_data("plotly_relayout"), {
    d <- event_data("plotly_relayout")
    # unfortunately, the data structure emitted is different depending on 
    # whether the relayout is triggered from the rangeslider or the plot
    xmin <- if (length(d[["xaxis.range[0]"]])) d[["xaxis.range[0]"]] else d[["xaxis.range"]][1]
    xmax <- if (length(d[["xaxis.range[1]"]])) d[["xaxis.range[1]"]] else d[["xaxis.range"]][2]
    if (is.null(xmin) || is.null(xmax)) return(NULL)
    
    # compute the y-range based on the new x-range
    idx <- with(txhousing, xmin <= date & date <= xmax)
    yrng <- extendrange(txhousing$median[idx])
    
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke("relayout", list(yaxis = list(range = yrng)))
  })
  
  yRange <- range(txhousing$median, na.rm = TRUE)
  observeEvent(event_data("plotly_doubleclick"), {
    
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke("relayout", list(yaxis = list(range = yRange)))
    
  })
  
  
}

shinyApp(ui, server)
