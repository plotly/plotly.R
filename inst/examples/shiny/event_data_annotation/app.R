library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("p"),
  checkboxInput("edit", "Enable edit mode? Capturing annotation click events in edit mode is not possible.", FALSE),
  verbatimTextOutput("data")
)

server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    plot_ly(mtcars) %>% 
      add_annotations(x = ~wt, y = ~mpg, text = row.names(mtcars), captureevents = TRUE)
  })
  
  observeEvent(input$edit, {
    plotlyProxy("p", session) %>%
      plotlyProxyInvoke("reconfig", editable = input$edit)
  })
  
  output$data <- renderPrint({
    event_data("plotly_clickannotation")
  })
}

shinyApp(ui, server)
