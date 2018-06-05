library(shiny)
library(plotly)
library(ggplot2)
library(promises)
library(future)
plan(multisession)

ui <- fluidPage(
  plotlyOutput("plot1"),
  plotlyOutput("plot2"),
  plotlyOutput("plot3"),
  plotlyOutput("plot4")
)

server <- function(input, output, session) {
  output$plot1 <- renderPlotly({
    # Async plot_ly
    future({ Sys.sleep(2); cars }) %...>%
      plot_ly(x = ~speed, y = ~dist, type = "scatter", mode = "markers")
  })
  
  output$plot2 <- renderPlotly({
    # Async ggplotly
    future({ Sys.sleep(2); mtcars }) %...>%
    { ggplot(., aes(hp, mpg)) + geom_point() } %...>%
      ggplotly()
  })
  
  output$plot3 <- renderPlotly({
    # Not async
    plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width,
            type = "scatter", mode = "markers")
  })
  
  output$plot4 <- renderPlotly({
    # Ensure errors are handled properly (should be blank)
    future({}) %...>%
    { req(FALSE) }
  })
}

shinyApp(ui, server)
