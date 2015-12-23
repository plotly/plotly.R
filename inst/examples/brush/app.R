library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

ui <- fluidPage(
  plotOutput("scatter1", brush = brushOpts("brush", direction = "xy")),
  plotlyOutput("plot1")
)

server <- function(input, output, session) {
  output$scatter1 <- renderPlot({
    iris$selected <- 1
    if (!is.null(input$brush)) { 
      br <- input$brush
      x <- iris$Sepal.Length <= br$xmin | br$xmax <= iris$Sepal.Length
      y <- iris$Sepal.Width <= br$ymin | br$ymax <= iris$Sepal.Width
      iris$selected[x | y] <- 0.3 
    }
    ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species, alpha = selected)) + 
      geom_point() + scale_alpha(guide = "none")
  })
  
  output$plot1 <- renderPlotly({
    p <- iris %>%
      count(Species) %>%
      plot_ly(x = Species, y = n, opacity = 0.5, type = "bar",
              marker = list(color = toRGB("grey92"))) %>%
      layout(barmode = "overlay", showlegend = FALSE)
    if (!is.null(input$brush)) { 
      br <- input$brush
      s <- iris %>%
        filter(br$xmin <= Sepal.Length, Sepal.Length <= br$xmax) %>%
        filter(br$ymin <= Sepal.Width, Sepal.Width <= br$ymax) %>%
        count(Species)
      p <- add_trace(p, x = Species, y = n, data = s,
                     marker = list(color = toRGB("steelblue")))
    }
    p
  })
}

shinyApp(ui, server)
