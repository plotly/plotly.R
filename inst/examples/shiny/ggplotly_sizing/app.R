library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("pid")
)

server <- function(input, output, session, ...) {
  
  # to relay the height/width of the plot's container, we'll query this 
  # session's client data http://shiny.rstudio.com/articles/client-data.html
  cdata <- session$clientData
  
  output$pid <- renderPlotly({
    p <- ggplot(iris) + 
      geom_point(aes(Sepal.Length, Sepal.Width)) +
      facet_wrap(~Species)
    
    ggplotly(p, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
  
}

shinyApp(ui, server)
