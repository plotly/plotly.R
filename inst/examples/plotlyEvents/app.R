library(shiny)
library(plotly)

ui <- fluidPage(
  radioButtons("plotType", "Plot Type:", choices = c("ggplotly", "plotly")),
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    if (identical(input$plotType, "ggplotly")) {
      p <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
      ggplotly(p) %>% layout(dragmode = "select")
    } else {
      plot_ly(mtcars, x = mpg, y = wt, mode = "markers") %>%
        layout(dragmode = "select")
    }
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

shinyApp(ui, server, options = list(display.mode = "showcase"))
