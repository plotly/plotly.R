library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("data")
)

mtcars$id <- row.names(mtcars)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~disp, y = ~mpg) %>%
      add_markers(key = ~id) %>%
      layout(dragmode = "select") %>%
      highlight("plotly_selected")
  })
  
  selected <- reactiveVal(rep(FALSE, nrow(mtcars)))
  
  selected_data <- reactive({
    ed <- event_data("plotly_selected")
    if (is.null(ed)) return(NULL)
    new <- mtcars[["id"]] %in% ed[["key"]]
    selected(selected() | new)
    mtcars[selected(), ]
  })
  
  output$data <- renderPrint({
    selected_data()
  })
  
}

shinyApp(ui, server)
