library(shiny)
library(plotly)
library(dplyr)

ui <- fluidPage(
  plotlyOutput("bars"),
  plotlyOutput("markers")
)

server <- function(input, output, session) {
  
  output$bars <- renderPlotly({
    plot_ly(diamonds, x = ~depth, source = "bars") %>%
      layout(
        dragmode = "select",
        selectdirection = "h"
      )
  })
  
  output$markers <- renderPlotly({
    plot_ly(diamonds, x = ~carat, y = ~price, alpha = 0.2) %>%
      toWebGL()
  })
  
  observe({
    brush <- event_data("plotly_brushing", source = "bars")
    if (is.null(brush)) brush <- list(x = range(diamonds$depth))
    
    is_within <- between(diamonds$depth, brush$x[1], brush$x[2])
    cols <- if_else(is_within, "rgba(31,119,180,0.2)", "transparent")
    
    plotlyProxy("markers", session) %>%
      plotlyProxyInvoke("restyle", "marker.color", list(cols))
  })
  
  
}

shinyApp(ui, server)
