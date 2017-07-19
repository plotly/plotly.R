library(shiny)
library(plotly)

ui <- fluidPage(
  selectInput("color", "Canada's fillcolor", colors(), selected = "black"),
  plotlyOutput("map")
)

server <- function(input, output, session) {
  
  output$map <- renderPlotly({
    
    map_data("world", "canada") %>%
      group_by(group) %>%
      plot_mapbox(x = ~long, y = ~lat, color = I("black")) %>%
      add_polygons() %>%
      layout(
        mapbox = list(
          center = list(lat = ~median(lat), lon = ~median(long))
        )
      )
    
  })
  
  observeEvent(input$color, {
    
    plotlyProxy("map", session) %>%
      plotlyProxyInvoke(
        "restyle", list(fillcolor = toRGB(input$color))
      )
    
  })
  
}

shinyApp(ui, server)
