
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

lm_app <- function(dat, x, y, key = row.names(dat)) {
  
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
  
  server <- function(input, output, session) {
    
    # mechanism for managing selected points
    keys <- reactiveVal()
    
    observeEvent(event_data("plotly_click"), {
      key_new <- event_data("plotly_click")$key
      key_old <- keys()
      
      if (key_new %in% key_old) {
        keys(setdiff(key_old, key_new))
      } else {
        keys(c(key_new, key_old))
      }
    })
    
    output$plot1 <- renderPlotly({
      req(input$degree)
      is_outlier <- key %in% keys()
      modelDat <- dat[!is_outlier, ]
      formula <- substitute(
        y ~ poly(x, degree = degree), 
        list(
          y = y[[2]],
          x = x[[2]],
          degree = input$degree
        )
      )
      m <- lm(formula, modelDat)
      modelDat$yhat <- as.numeric(fitted(m))
      
      cols <- ifelse(is_outlier, "gray90", "black")
      
      dat %>%
        plot_ly(x = ~wt, y = ~mpg) %>%
        add_markers(key = row.names(mtcars), color = I(cols), marker = list(size = 10)) %>%
        add_lines(y = ~yhat, data = modelDat) %>%
        layout(showlegend = FALSE)
    })
    
    # Return the most recent fitted model, when we press "done"
    observeEvent(input$done, {
      modelDat <- dat[!key %in% keys(), ]
      formula <- as.formula(
        sprintf("%s ~ poly(%s, degree = %s)", as.character(y)[2], as.character(x)[2], input$degree)
      )
      m <- lm(formula, modelDat)
      print(summary(m))
      stopApp(m)
    })
  }
  
  shinyApp(ui, server)
}

lm_app(mtcars, x = ~wt, y = ~mpg)
