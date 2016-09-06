library(shiny)
library(plotly)

reusableUI <- function(id = NULL) {
  ns <- NS(id)
  
  fluidRow(
    column(4, plotlyOutput(ns("p1"))),
    column(4, plotlyOutput(ns("p2"))),
    column(4, verbatimTextOutput(ns("ev1"))),
    column(4, verbatimTextOutput(ns("ev2")))
  )
}

viz <- function(input, output, session, src) {
  
  # if you want, you can distinguish between events *within* a module
  src2 <- paste0(src, "2")
  
  output$p1 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~disp, 
            key = row.names(mtcars), source = src)
  })
  output$p2 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~disp, 
            key = row.names(mtcars), source = src2)
  })
  output$ev1 <- renderPrint({
    event_data("plotly_hover", source = src)
  })
  output$ev2 <- renderPrint({
    event_data("plotly_hover", source = src2)
  })
  
}

ui <- fluidPage(
  reusableUI("one"),
  reusableUI("two")
)

server <- function(input, output, session) {
  # use the src argument to namespace plotly events
  callModule(viz, "one", src = "A")
  callModule(viz, "two", src = "B")
}

shinyApp(ui, server)
