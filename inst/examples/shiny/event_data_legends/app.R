library(shiny)

ui <- fluidPage(
  plotlyOutput("gg"),
  verbatimTextOutput("click"),
  verbatimTextOutput("doubleclick")
)

server <- function(input, output, session) {
  
  output$gg <- renderPlotly({
    ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
      geom_point() + 
      facet_wrap(~vs)
  })
  
  output$click <- renderPrint({
    event_data("plotly_legendclick")
  })
  
  output$doubleclick <- renderPrint({
    event_data("plotly_legenddoubleclick")
  })
}

shinyApp(ui, server)
