library(plotly)
library(purrr)
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(5, verbatimTextOutput("summary")),
    column(7, plotlyOutput("p"))
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    x = mtcars$mpg,
    y = mtcars$wt
  )
  grid <- reactive({
    data.frame(x = seq(min(rv$x), max(rv$x), length = 10))
  })
  model <- reactive({
    d <- data.frame(x = rv$x, y = rv$y)
    lm(y ~ x, d)
  })
  
  output$p <- renderPlotly({
    # creates a list of circle shapes from x/y data
    circles <- map2(rv$x, rv$y, 
      ~list(
        type = "circle",
        # anchor circles at (mpg, wt)
        xanchor = .x,
        yanchor = .y,
        # give each circle a 2 pixel diameter
        x0 = -4, x1 = 4,
        y0 = -4, y1 = 4,
        xsizemode = "pixel", 
        ysizemode = "pixel",
        # other visual properties
        fillcolor = "blue",
        line = list(color = "transparent")
      )
    )
    
    # plot the shapes and fitted line
    plot_ly() %>%
      add_lines(x = grid()$x, y = predict(model(), grid()), color = I("red")) %>%
      layout(shapes = circles) %>%
      config(edits = list(shapePosition = TRUE))
  })
  
  output$summary <- renderPrint({a
    summary(model())
  })
  
  # update x/y reactive values in response to changes in shape anchors
  observe({
    ed <- event_data("plotly_relayout")
    shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
    if (length(shape_anchors) != 2) return()
    row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
    pts <- as.numeric(shape_anchors)
    rv$x[row_index] <- pts[1]
    rv$y[row_index] <- pts[2]
  })
  
}

shinyApp(ui, server)
