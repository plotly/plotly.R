library(shiny)
library(plotly)

ui <- fluidPage(
  radioButtons("plotType", "Plot Type:", choices = c("ggplotly", "plotly")),
  plotlyOutput("plot"),
  verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  
  nms <- row.names(mtcars)
  
  output$plot <- renderPlotly({
    if (identical(input$plotType, "ggplotly")) {
      p <- ggplot(mtcars, aes(x = mpg, y = wt, key = nms)) + geom_point()
      ggplotly(p) %>% layout(dragmode = "select")
    } else {
      plot_ly(mtcars, x = ~mpg, y = ~wt, key = nms) %>%
        layout(dragmode = "select")
    }
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
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

shinyApp(ui, server, options = list(display.mode = "showcase"))
