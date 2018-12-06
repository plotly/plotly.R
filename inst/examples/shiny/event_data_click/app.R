library(plotly)
library(shiny)

# cache computation of a correlation matrix
correlation <- round(cor(mtcars), 3)

ui <- fluidPage(
  mainPanel(
    plotlyOutput("heat"),
    plotlyOutput("scatterplot")
  ),
  verbatimTextOutput("selection")
)

server <- function(input, output, session) {
  
  output$heat <- renderPlotly({
    plot_ly(source = "heatmap") %>%
      add_heatmap(
        x = names(mtcars), 
        y = names(mtcars), 
        z = correlation
      ) %>%
      layout(
        xaxis = list(title = ""), 
        yaxis = list(title = "")
      )
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click", source = "heatmap")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$scatterplot <- renderPlotly({
    clickData <- event_data("plotly_click", source = "heatmap")
    if (is.null(clickData)) return(NULL)
    
    # get the clicked x/y variables and fit model to those 2 vars
    vars <- c(clickData[["x"]], clickData[["y"]])
    d <- setNames(mtcars[vars], c("x", "y"))
    yhat <- fitted(lm(y ~ x, data = d))
    
    # scatterplot with fitted line
    plot_ly(d, x = ~x) %>%
      add_markers(y = ~y) %>%
      add_lines(y = ~yhat) %>%
      layout(
        xaxis = list(title = clickData[["x"]]), 
        yaxis = list(title = clickData[["y"]]), 
        showlegend = FALSE
      )
  })
  
}

shinyApp(ui, server, options = list(display.mode = "showcase"))
