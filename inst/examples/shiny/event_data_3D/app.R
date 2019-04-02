library(plotly)
library(shiny)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  verbatimTextOutput("legendclick"),
  verbatimTextOutput("legend2click"),
  verbatimTextOutput("relayout")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~wt, y = ~mpg, z = ~disp, color = ~factor(cyl)) %>%
      event_register("plotly_legendclick") %>%
      event_register("plotly_legenddoubleclick") 
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here" else d
  })
  
  output$legendclick <- renderPrint({
    d <- event_data("plotly_legendclick")$name
    if (is.null(d)) "Legend click" else d
  })
  
  output$legend2click <- renderPrint({
    d <- event_data("plotly_legenddoubleclick")$name
    if (is.null(d)) "Legend double-click" else d
  })
  
  output$relayout <- renderPrint({
    d <- event_data("plotly_relayout")$scene.camera$eye
    if (is.null(d)) "Camera eye info" else d
  })
  
}

shinyApp(ui, server)
