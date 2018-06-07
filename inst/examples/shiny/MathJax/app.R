# Example of rendering both plotly's (SVG) MathJax inside a 
# shiny app with it's own (HTML) MathJax. This currently requires 
# bringing in the plotly graph with an iframe
library(shiny)
library(plotly)
library(htmlwidgets)
library(htmltools)

addResourcePath("tmp", tempdir())
f <- tempfile(fileext = ".html")

ui <- fluidPage(
  withMathJax(helpText('Dynamic output 1:  $$\\alpha^2$$')),
  uiOutput("p")
)

server <- function(input, output, ...) {
  
  output$p <- renderUI({
    
    plot_ly() %>%
      add_lines(x = zoo::index(co2), y = co2) %>%
      layout(
        title = TeX("CO_2 \\text{measured in } \\frac{parts}{million}"),
        xaxis = list(title = "Time"),
        yaxis = list(title = TeX("\\text{Atmospheric concentraion of CO}_2"))
      ) %>%
      config(mathjax = "cdn") %>%
      saveWidget(f)
    
    tags$iframe(
      src = file.path("tmp", basename(f)),
      width = "100%", 
      height = "400",
      scrolling = "no", 
      seamless = "seamless", 
      frameBorder = "0"
    )
  })
  
}

shinyApp(ui, server)
