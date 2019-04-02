library(shiny)
library(plotly)
library(dplyr)
library(MASS)

kde_full <- with(diamonds, kde2d(carat, price))

ui <- fluidPage(
  plotlyOutput("bars", height = 300),
  plotlyOutput("heat")
)

server <- function(input, output, session) {
  
  output$bars <- renderPlotly({
    plot_ly(diamonds, x = ~depth, source = "bars") %>%
      layout(dragmode = "select", selectdirection = "h")
  })
  
  output$heat <- renderPlotly({
    plot_ly() %>%
      add_heatmap(x = kde_full$x, y = kde_full$y, z = sqrt(t(kde_full$z)))
  })
  
  observe({
    brush <- event_data("plotly_brushing", source = "bars")
    p <- plotlyProxy("heat", session)
    
    # show full data if no brush exists
    if (is.null(brush)) {
      plotlyProxyInvoke(p, "restyle", "z", list(sqrt(t(kde_full$z))))
      return()
    }
    
    d_filter <- filter(diamonds, between(depth, brush$x[1], brush$x[2]))
    if (nrow(d_filter) < 10) return()
    
    kde_filter <- with(d_filter, kde2d(carat, price))
    
    plotlyProxyInvoke(p, "restyle", "z", list(sqrt(t(kde_filter$z))))
  })
  
}

shinyApp(ui, server)
