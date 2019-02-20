library(shiny)
library(plotly)
library(dplyr)
library(crosstalk)

ui <- fluidPage(
  checkboxInput("persist", "Persistent?", FALSE),
  plotlyOutput("p"),
  verbatimTextOutput("dat")
)

server <- function(input, output, session) {

  # change these variables to customize the app
  data_shared <- SharedData$new(mtcars)
  color_base <- "black"
  color_select <- "red"
  var_x <- "wt"
  var_y <- "mpg"

  data_orig <- data_shared$origData()

  output$p <- renderPlotly({
    plot_ly(x = data_orig[[var_x]], y = data_orig[[var_y]]) %>%
      add_markers(
        color = I(color_base),
        selected = list(
          marker = list(color = color_select)
        )
      ) %>%
      layout(
        dragmode = "select",
        xaxis = list(title = var_x),
        yaxis = list(title = var_y)
      ) %>%
      config(
        displayModeBar = FALSE,
        edits = list(shapePosition = TRUE)
      ) %>%
      toWebGL()
  })

  # listen to the brushing event and draw a
  # rect shape that mimics the brush
  observe({
    brush <- event_data("plotly_brushing")

    # if the brush is undefined, remove all shapes and exit
    if (is.null(brush)) {
      plotlyProxy("p", session) %>%
        plotlyProxyInvoke("relayout", list(shapes = NULL))
      return()
    }

    # mimc the brush as a rect shape
    brush_rect <- list(
      type = "rect",
      x0 = brush$x[1],
      x1 = brush$x[2],
      y0 = brush$y[1],
      y1 = brush$y[2],
      fillcolor = NA,
      line = list(
        color = "black",
        dash = "dot",
        width = 1
      )
    )

    # draw the rect shape and turn off brush coloring
    # imposed by plotly.js
    plotlyProxy("p", session) %>%
      plotlyProxyInvoke("relayout", list(shapes = list(brush_rect))) %>%
      plotlyProxyInvoke("restyle", "selectedpoints", list(list()))
  })

  # A reactive value that tracks the dimensions of the brush
  brush <- reactiveVal()

  # Update the brush in response to changes to shapes
  # NOTE: if you need more shapes in the plot your brushing,
  # you'll need to be mindful of which shape the brush represents
  observe({
    evt <- event_data("plotly_relayout")
    val <- if (!is.null(evt$shapes)) {
      evt$shapes
    } else if (!is.null(evt[["shapes[0].x0"]])) {
      list(
        x0 = evt[["shapes[0].x0"]],
        x1 = evt[["shapes[0].x1"]],
        y0 = evt[["shapes[0].y0"]],
        y1 = evt[["shapes[0].y1"]]
      )
    }
    brush(val)
  })

  # double-click clears the brush
  observe({
    event_data("plotly_doubleclick", priority = "event")
    event_data("plotly_deselect", priority = "event")
    brush(NULL)
  })

  # map the brush limits to a data selection
  observe({

    # if brush isn't active, no selection is active
    if (is.null(brush())) {
      data_shared$selection(FALSE)
      return()
    }

    selection <- between(data_orig[[var_x]], brush()$x0, brush()$x1) &
      between(data_orig[[var_y]], brush()$y0, brush()$y1)

    if (isTRUE(input$persist)) {
      selection_old <- data_shared$selection()
      # This should be fixed in crosstalk
      if (is.null(selection_old)) selection_old <- FALSE
      data_shared$selection(selection_old | selection)
    } else {
      data_shared$selection(selection)
    }
  })

  # update the marker colors
  observe({
    dat <- data_shared$data(withSelection = TRUE)
    color <- if_else(dat$selected_, color_select, color_base)
    plotlyProxy("p", session) %>%
      plotlyProxyInvoke("restyle", "marker.color", list(color), 0)
  })

  # display the selected data
  output$dat <- renderPrint({
    dat <- data_shared$data(withSelection = TRUE)
    filter(dat, selected_)
  })
}

shinyApp(ui, server)
