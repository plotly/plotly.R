library(shiny)

ui <- fluidPage(
  plotlyOutput("p"),
  textOutput("time1"),
  textOutput("time2")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(x = 1:2, y = 1:2, size = I(c(100, 150)))  %>%
      add_markers()
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
