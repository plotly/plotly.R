# see https://community.rstudio.com/t/sliding-a-point-on-a-plot-rather-than-sliderinput-to-update-plot-in-shiny/16405/7
library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("p"),
  verbatimTextOutput("event")
)

x <- seq(-10, 10)
y <- rnorm(length(x))


server <- function(input, output, session) {
  
  output$p <- renderPlotly({
    
    d <- event_data("plotly_relayout", source = "trajectory")
    
    selected_point <- if (!is.null(d[["shapes[0].x0"]])) {
      xint <- d[["shapes[0].x0"]]
      xpt <- x[which.min(abs(x - xint))]
      list(x = xpt, y = y[which(x == xpt)])
    } else {
      list(x = 1, y = y[which(x == 1)])
    }
    
    plot_ly(color = I("red"), source = "trajectory") %>%
      add_lines(x = x, y = y) %>%
      add_markers(x = selected_point$x, y = selected_point$y) %>%
      layout(
        shapes = list(
          type = "line", 
          line = list(color = "gray", dash = "dot"),
          x0 = selected_point$x, 
          x1 = selected_point$x,
          y0 = 0,
          y1 = 1,
          yref = "paper"
        )
      ) %>%
      config(editable = TRUE)
  })
  
}

shinyApp(ui, server)
