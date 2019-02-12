library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("p"),
  textOutput("time1"),
  textOutput("time2")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(x = 1, y = 1, marker = list(size = 100))  %>%
      add_trace(text = "Click me", mode = "markers+text")
  })
  
  output$time1 <- renderText({
    event_data("plotly_click")
    paste("Input priority: ", Sys.time())
  })
  
  output$time2 <- renderText({
    event_data("plotly_click", priority = "event")
    paste("Event priority: ", Sys.time())
  })
  
}

shinyApp(ui, server)
