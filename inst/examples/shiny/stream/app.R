library(shiny)
library(plotly)

ui <- fluidPage(
  actionButton("stream", "Turn stream on/off"),
  plotlyOutput("plot")
)

server <- function(input, output, session) {
  
  # initial values
  yint <- c(0, 1)
  
  # initiate graph with initial values
  output$plot <- renderPlotly({
    plot_ly(y = yint, x = seq_along(yint)) %>%
      add_lines()
  })
  
  # reactiveValues() act very much like input values, but may be used to 
  # maintain state (e.g., are we currently streaming?)
  rv <- reactiveValues(
    stream = FALSE,
    yend = sum(yint), 
    n = length(yint)
  )
  
  # turn streaming on/off when the button is pressed
  observeEvent(input$stream, {
    rv$stream <- if (rv$stream) FALSE else TRUE
  })
  
  observe({
    # if we're not streaming, don't do anything
    if (!rv$stream) return()
    
    # re-execute this code block to every 100 milliseconds
    invalidateLater(100, session)
    # changing a reactive value "invalidates" it, so isolate() is needed 
    # to avoid recursion
    isolate({
      rv$n <- rv$n + 1
      rv$yend <- rv$yend + sample(c(-1, 1), 1)
    })
    
    # add the new value to the plot
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke(
        "extendTraces", 
        list(
          y = list(list(rv$yend)), 
          x = list(list(rv$n))
        ), 
        list(0)
      )
  })
  
}

shinyApp(ui, server)
