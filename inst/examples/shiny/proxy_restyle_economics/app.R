library(shiny)
library(plotly)

ui <- fluidPage(
  sliderInput("marker", "Marker size", min = 0, max = 20, value = 8),
  sliderInput("path", "Path size", min = 0, max = 30, value = 2),
  plotlyOutput("p")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(
      economics, x = ~pce, y = ~psavert, z = ~unemploy, 
      color = ~as.numeric(date), mode = "markers+lines"
    )
  })
  
  observeEvent(input$marker, {
    plotlyProxy("p", session) %>%
      plotlyProxyInvoke(
        "restyle", 
        # could also do list(marker = list(size = input$marker))
        # but that overwrites the existing marker definition
        # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
        list(marker.size = input$marker)
      )
  })
  
  observeEvent(input$path, {
    plotlyProxy("p", session) %>%
      plotlyProxyInvoke(
        "restyle", list(line.width = input$path)
      )
  })
  
}

shinyApp(ui, server)
