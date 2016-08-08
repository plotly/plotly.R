library(plotly)
library(shiny)
library(crosstalk)
library(dplyr)

# Prepare mtcars
mtcars2 <- mtcars
mtcars2$gear <- factor(mtcars2$gear)
mtcars2$cyl <- factor(mtcars2$cyl)
# Move rownames to a proper column
mtcars2 <- mtcars2 %>% tibble::rownames_to_column()

sd <- SharedData$new(mtcars2, ~rowname)

ui <- fluidPage(
  fillRow(height = 500,
    plotlyOutput("p1"),
    plotlyOutput("p2"),
    plotOutput("plot1")
  ),
  crosstalk::filter_select("gears", "Gears", sd, ~gear),
  crosstalk::filter_select("cyl", "Cylinders", sd, ~cyl)
)

server <- function(input, output, session) {
  
  output$p1 <- renderPlotly({
    plot_ly(sd, x = ~wt, y = ~mpg, color = ~gear, height = "100%")
  })
  
  output$p2 <- renderPlotly({
    plot_ly(sd, x = ~wt, y = ~disp, color = ~gear, height = "100%")
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
