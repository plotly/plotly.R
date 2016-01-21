library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = mpg, y = wt, mode = "markers") %>%
      layout(dragmode =  "select")
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a point to view event data" else d
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Click and drag to view event data" else d
  })
  
}

shinyApp(ui, server)
