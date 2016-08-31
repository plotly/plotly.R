library(shiny)
library(plotly)

reusableUI <- function(id = NULL) {
  ns <- NS(id)
  
  fluidRow(
    column(4, plotlyOutput(ns("p1"))),
    column(4, plotlyOutput(ns("p2"))),
    column(4, verbatimTextOutput(ns("ev")))
  )
}

viz <- function(input, output, session) {
  output$p1 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~disp, 
            key = row.names(mtcars), session = session)
  })
  output$p2 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~disp, 
            key = row.names(mtcars), session = session)
  })
  output$ev <- renderPrint({
    d <- event_data("plotly_hover", session = session)
    if (is.null(d)) print(paste("Module", session$ns(NULL))) else d
  })
}

ui <- fluidPage(
  reusableUI("one"),
  reusableUI("two")
)

server <- function(input, output, session) {
  callModule(viz, "one", session = session)
  callModule(viz, "two", session = session)
}

shinyApp(ui, server)
