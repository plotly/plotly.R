library(plotly)
library(shiny)
library(htmltools)
library(ggplot2)
library(crosstalk)
library(dplyr)

ui <- fluidPage(
  fillRow(height = 500,
    plotlyOutput("p1"),
    plotlyOutput("p2"),
    plotOutput("plot1")
  )
)

server <- function(input, output, session) {

  # Prepare mtcars
  mtcars$gear <- factor(mtcars$gear)
  mtcars$cyl <- factor(mtcars$cyl)
  # Move rownames to a proper column
  mtcars <- mtcars %>% add_rownames()
  
  sd <- SharedData$new(mtcars, "rowname", group = "A")
  
  output$p1 <- renderPlotly({
    plot_ly(mtcars, x = wt, y = mpg, color = cyl, mode = "markers", 
      key = mtcars$rowname, set = "A", height = "100%") %>%
      add_trace(x = wt, y = hp, mode = "markers", key = mtcars$rowname, set = "B") %>%
      layout(dragmode = "select")
  })
  
  output$p2 <- renderPlotly({
    plot_ly(mtcars, x = wt, y = disp, color = cyl, mode = "markers", 
      key = mtcars$rowname, set = "A", height = "100%") %>%
      add_trace(x = wt, y = hp, mode = "markers", key = mtcars$rowname, set = "B") %>%
      layout(dragmode = "select")
  })
  
  output$plot1 <- renderPlot({
    df <- sd$data(withSelection = TRUE)
    
    # If nothing selected, set all to TRUE
    if (all(is.na(df$selected_)))
      df$selected_ <- TRUE
    
    # Use ordered factor levels, otherwise the highlighted parts
    # of the bars appear on the top, not the bottom
    df$selected_ <- factor(df$selected_, levels = c("TRUE", "FALSE"))
    
    ggplot(df, aes(x = cyl)) +
      # Specify TRUE/FALSE colors, and hide legend
      scale_fill_manual(values = c("#000000", "#CCCCCC"), guide = FALSE) +
      geom_bar(stat = "count", aes(fill = selected_))
  })
}

shinyApp(ui, server)
