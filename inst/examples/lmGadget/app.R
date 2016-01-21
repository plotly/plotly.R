# Many thanks to RStudio for shiny gadgets
# And special thanks to Winston Chang for the inspiration
# https://gist.github.com/wch/c4b857d73493e6550cba
library(shiny)
library(miniUI)
library(plotly)

#' Shiny gadget for interactive linear model fitting
#' 
#' Click on points to add/remove them from consideration
#' 
#' @param dat a data.frame
#' @param x a character string specifying the x variable
#' @param y a character string specifying the y variable

lmGadget <- function(dat, x, y) {
  
  ui <- miniPage(
    gadgetTitleBar("Interactive lm"),
    miniContentPanel(
      fillRow(
        flex = c(NA, 1),
        fillCol(
          width = "100px",
          selectInput("degree", "Polynomial degree", c(1, 2, 3, 4))
        ),
        plotlyOutput("plot1", height = "100%")
      )
    )
  )
  
  # mechanism for managing selected points
  init <- function() {
    selected <- rep(FALSE, nrow(dat))
    function(x) {
      selected <<- xor(selected, x)
      selected
    }
  }
  selection <- init()
  
  server <- function(input, output) {
    
    # obtain a subset of the data that is still under consideration
    left <- reactive({
      d <- event_data("plotly_click")
      if (!is.null(d)) {
        dat <- dat[!selection(row.names(dat) %in% d[["key"]]), ]
      }
      dat
    })
    
    # fit a model to subsetted data
    refit <- reactive({
      req(input$degree)
      formula <- as.formula(
        sprintf("%s ~ poly(%s, degree = %s)", y, x, input$degree)
      )
      lm(formula, left())
    })

    output$plot1 <- renderPlotly({
      dat2 <- left()
      dat2$yhat <- as.numeric(fitted(refit()))
      # sort data by 'x' variable so we draw a line (not a path)
      dat2 <- dat2[order(dat2[, x]), ]
      
      plot_ly(x = dat[, x], y = dat[, y], key = row.names(dat), mode = "markers",
              marker = list(color = toRGB("grey90"), size = 10)) %>%
        add_trace(x = dat2[, x], y = dat2[, y], mode = "markers",
                  marker = list(color = toRGB("black"), size = 10)) %>%
        add_trace(x = dat2[, x], y = dat2$yhat, mode = "lines",
                  marker = list(color = toRGB("black"))) %>%
        layout(showlegend = FALSE, xaxis = list(title = x), yaxis = list(title = y))
    })
    
    # Return the most recent fitted model, when we press "done"
    observeEvent(input$done, {
      stopApp(refit())
    })
  }
  
  runGadget(ui, server)
}

m <- lmGadget(mtcars, "wt", "mpg")
