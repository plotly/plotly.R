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


# brushing on a static ggplot and targeting a plotly
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
  
  sd <- crosstalk::SharedData$new(iris %>% add_rownames(), "rowname", group = "A")
  
  output$plot1 <- renderPlotly({
    df <- sd$data(TRUE)
    d <- count(iris, Species)
    p <- iris %>%
      count(Species) %>%
      plot_ly(x = Species, y = n, opacity = 0.5, type = "bar") %>%
      layout(barmode = "overlay", showlegend = FALSE)
    if (all(!is.na(df$selected_))) { 
      s <- df %>%
        filter(selected_) %>%
        count(Species)
      p <- add_trace(p, x = Species, y = n, data = s)
    }
    p
  })
  
  observeEvent(input$brush, {
    df <- brushedPoints(sd$data(FALSE), input$brush, allRows = TRUE)
    selected <- row.names(df)[df$selected_]
    sd$selection(selected)
  }, ignoreNULL = FALSE)
}

shinyApp(ui, server)
