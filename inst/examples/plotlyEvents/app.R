library(shiny)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    mtcars %>%
      plot_ly(x = ~mpg, y = ~wt, key = row.names(mtcars)) %>%
      add_markers() %>%
      layout(dragmode = "select")
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
  })
  
}

shinyApp(ui, server)
