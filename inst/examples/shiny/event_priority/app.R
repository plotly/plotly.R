library(plotly)
library(shiny)

ui <- fluidPage(
  checkboxInput("priority", "Shiny event priority", FALSE),
  plotlyOutput("p")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    title <- if (input$priority) {
      "Clicking on the same point repeatedly will keep triggering console output"
    } else {
      "Clicking on the same point won't trigger more output"
    }
    
    plot_ly(mtcars, x = ~wt, y = ~mpg) %>%
      layout(title = title) %>%
      config(priority = if (input$priority) "event")
  })
  
  observeEvent(event_data("plotly_click"), {
    print("clicked!")
  })
  
}

shinyApp(ui, server)
