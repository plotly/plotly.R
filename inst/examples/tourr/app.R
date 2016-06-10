# modified from https://github.com/rstudio/ggvis/blob/master/demo/tourr.r

library(tourr)
library(plotly)
library(shiny)

aps <- 2
fps <- 30

mat <- rescale(as.matrix(flea[1:6]))
tour <- new_tour(mat, grand_tour(), NULL)
start <- tour(0)

ui <- fluidPage(
  plotlyOutput("p")
)

server <- function(input, output) {
  
  proj_data <- reactive({
    invalidateLater(1000 / fps, NULL);
    step <- tour(aps / fps)
    data.frame(center(mat %*% step$proj), species = flea$species)
  })
  
  output$p <- renderPlotly({
    proj_data() %>%
      plot_ly(x = X1, y = X2, color = species, mode = "markers") %>%
      layout(xaxis = list(range = c(-1, 1)), yaxis = list(range = c(-1, 1)))
  })
}

shinyApp(ui, server)
