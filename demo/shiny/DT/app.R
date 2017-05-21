library(plotly)
library(DT)
library(shiny)
library(crosstalk)

m <- mtcars[, c("mpg", "wt", "disp")] %>% 
  tibble::rownames_to_column()

ui <- fluidPage(
  plotlyOutput("plots"),
  DT::dataTableOutput("table")
)

server <- function(input, output) {
  
  d <- SharedData$new(m, ~rowname)
  
  output$plots <- renderPlotly({
    subplot(
      qplot(data = d, x = mpg, y = wt),
      qplot(data = d, x = mpg, y = disp),
      titleX = T, titleY = T, margin = 0.03
    ) %>% highlight("plotly_selected")
  })
  
  output$table <- DT::renderDataTable({
    m2 <- m[d$selection(),]
    dt <- DT::datatable(m)
    if (NROW(m2) == 0) {
      dt
    } else {
      DT::formatStyle(dt, "rowname", target = "row",
                      color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                      backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
    }
  })
}

shinyApp(ui = ui, server = server)
