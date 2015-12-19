library(plotly)
library(htmltools)

# hello click/show selects
mtcars$gear <- factor(mtcars$gear)
mtcars$cyl <- factor(mtcars$cyl)
p1 <- plot_ly(mtcars, x = wt, y = mpg, color = gear, mode = "markers", 
              key = gear, set = "A", width = 400)
p2 <- plot_ly(mtcars, x = wt, y = disp, color = gear, mode = "markers", 
              key = gear, set = "A", width = 400)
# TODO: inline-block?
browsable(tagList(
  as.widget(p1),
  as.widget(p2)
))

library(dplyr)
m <- count(mtcars, cyl)
p1 <- plot_ly(m, x = cyl, y = n, type = "bar", 
              key = cyl, set = "A", width = 400)
p2 <- plot_ly(mtcars, x = mpg, y = disp, mode = "markers", 
              key = cyl, set = "A", width = 400)
browsable(tagList(
  as.widget(p1),
  as.widget(p2)
))

# access renderPlotly() selections on a shiny server
library(shiny)
library(plotly)

mtcars$gear <- factor(mtcars$gear)

ui <- fluidPage(
  plotlyOutput("scatter1"),
  textOutput("stuff")
)

server <- function(input, output, session) {
  output$scatter1 <- renderPlotly({
    plot_ly(mtcars, x = wt, y = mpg, color = gear, mode = "markers", 
            key = gear, set = "A", width = 400)
  })
  rv <- reactive({
    cv <- crosstalk::ClientValue$new("tdb")
    cv$get()
  })
  output$stuff <- renderPrint({
    rv()
  })
}

shinyApp(ui, server)


# TODO: pass selections from another htmlwidget (rcdimple?) to renderPlotly()


# this actually need crosstalk, but might be interesting to compare the difference
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
    ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
      geom_point()
  })
  
  output$plot1 <- renderPlotly({
    p <- iris %>%
      count(Species) %>%
      plot_ly(x = Species, y = n, opacity = 0.5, type = "bar") %>%
      layout(barmode = "overlay", showlegend = FALSE)
    if (!is.null(input$brush)) { 
      br <- input$brush
      s <- iris %>%
        filter(br$xmin <= Sepal.Length, Sepal.Length <= br$xmax) %>%
        filter(br$ymin <= Sepal.Width, Sepal.Width <= br$ymax) %>%
        count(Species)
      p <- add_trace(p, x = Species, y = n, data = s)
    }
    p
  })
}

shinyApp(ui, server)

