library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("hover"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    plot_ly(x = rnorm(10), y = rnorm(10), z = rnorm(10), type = "scatter3d")
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
}

shinyApp(ui, server, options = list(display.mode = "showcase"))
