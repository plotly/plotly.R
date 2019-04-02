library(shiny)
library(plotly)

ui <- fluidPage(
  radioButtons("plotType", "Plot Type:", choices = c("ggplotly", "plotly")),
  plotlyOutput("plot"),
  verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brushing"),
  verbatimTextOutput("selecting"),
  verbatimTextOutput("brushed"),
  verbatimTextOutput("selected")
)

server <- function(input, output, session) {
  
  nms <- row.names(mtcars)
  
  output$plot <- renderPlotly({
    p <- if (identical(input$plotType, "ggplotly")) {
      ggplotly(ggplot(mtcars, aes(x = mpg, y = wt, customdata = nms)) + geom_point())
    } else {
      plot_ly(mtcars, x = ~mpg, y = ~wt, customdata = nms)
    }
    p %>% 
      layout(dragmode = "select") %>%
      event_register("plotly_selecting")
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  output$brushing <- renderPrint({
    d <- event_data("plotly_brushing")
    if (is.null(d)) "Brush extents appear here (double-click to clear)" else d
  })
  
  output$selecting <- renderPrint({
    d <- event_data("plotly_selecting")
    if (is.null(d)) "Brush points appear here (double-click to clear)" else d
  })
  
  output$brushed <- renderPrint({
    d <- event_data("plotly_brushed")
    if (is.null(d)) "Brush extents appear here (double-click to clear)" else d
  })
  
  output$selected <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Brushed points appear here (double-click to clear)" else d
  })
  
}

shinyApp(ui, server, options = list(display.mode = "showcase"))
