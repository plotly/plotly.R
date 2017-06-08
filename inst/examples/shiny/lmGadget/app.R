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
#' @param x a formula specifying the x variable
#' @param y a formula specifying the y variable
#' @param key a vector specifying unique attributes for each row

lmGadget <- function(dat, x, y, key = row.names(dat)) {
  
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
      if (missing(x)) return(selected)
      selected <<- xor(selected, x)
      selected
    }
  }
  selection <- init()
  
  server <- function(input, output) {

    output$plot1 <- renderPlotly({
      req(input$degree)
      d <- event_data("plotly_click")
      selected <- selection(key %in% d[["key"]])
      modelDat <- dat[!selected, ]
      formula <- as.formula(
        sprintf("%s ~ poly(%s, degree = %s)", as.character(y)[2], as.character(x)[2], input$degree)
      )
      m <- lm(formula, modelDat)
      modelDat$yhat <- as.numeric(fitted(m))
      mcolor <- rep(toRGB("black"), NROW(dat))
      mcolor[selected] <- toRGB("grey90")
      
      dat %>%
        plot_ly(x = x, y = y) %>%
        add_markers(key = key, marker = list(color = mcolor, size = 10)) %>%
        add_lines(y = ~yhat, data = modelDat) %>%
        layout(showlegend = FALSE)
    })
    
    # Return the most recent fitted model, when we press "done"
    observeEvent(input$done, {
      selected <- selection()
      modelDat <- dat[!selected, ]
      formula <- as.formula(
        sprintf("%s ~ poly(%s, degree = %s)", as.character(y)[2], as.character(x)[2], input$degree)
      )
      stopApp(lm(formula, modelDat))
    })
  }
  
  runGadget(ui, server)
}

m <- lmGadget(mtcars, x = ~wt, y = ~mpg)
