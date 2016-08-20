# UPDATE: this sort of thing is now possible without shiny,
# see inst/examples/crosstalk/07-binned-target.R
library(plotly)
library(shiny)
library(crosstalk)
library(dplyr)

ui <- fluidPage(
  fillRow(height = 500,
    plotlyOutput("p1"),
    plotlyOutput("p2"),
    plotOutput("plot1")
  ),
  filter_select("gears", "Gears", sd, ~gear),
  filter_select("cyl", "Cylinders", sd, ~cyl)
)

server <- function(input, output, session) {
  
  # Prepare mtcars
  sd <- mtcars %>%
    mutate(gear = factor(gear)) %>%
    mutate(cyl = factor(cyl)) %>%
    tibble::rownames_to_column() %>%
    SharedData$new(~rowname)
  
  output$p1 <- renderPlotly({
    plot_ly(sd, x = ~wt, y = ~mpg, color = ~gear, height = "100%")
  })
  
  output$p2 <- renderPlotly({
    plot_ly(sd, x = ~wt, y = ~disp, color = ~gear, height = "100%")
  })
  
  output$plot1 <- renderPlot({
    
    mtcars$selected_ <- if (any(sd$selection())) sd$selection() else FALSE
    
    # Use ordered factor levels, otherwise the highlighted parts
    # of the bars appear on the top, not the bottom
    mtcars$selected_ <- factor(mtcars$selected_, levels = c("TRUE", "FALSE"))
    
    ggplot(mtcars, aes(x = cyl)) +
      # Specify TRUE/FALSE colors, and hide legend
      scale_fill_manual(values = c("#000000", "#CCCCCC"), guide = FALSE) +
      geom_bar(stat = "count", aes(fill = selected_))
  })
}

shinyApp(ui, server)
