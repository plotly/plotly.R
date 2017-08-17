library(shiny)
library(plotly)

# get all the available mapbox styles
mapStyles <- schema()$layout$layoutAttributes$mapbox$style$values

ui <- fluidPage(
  selectInput("style", "Select a mapbox style", mapStyles),
  plotlyOutput("map")
)

server <- function(input, output, session) {
  
  output$map <- renderPlotly({
    plot_mapbox()
  })
  
  observeEvent(input$style, {
    plotlyProxy("map", session) %>%
      plotlyProxyInvoke(
        "relayout",
        list(mapbox = list(style = input$style))
      )
  })
  
}

shinyApp(ui, server)
