library(plotly)
library(shiny)

ui <- fluidPage(
  plotlyOutput("parcoords"),
  verbatimTextOutput("data")
)

server <- function(input, output, session) {
  
  iris_numeric <- dplyr::select_if(iris, is.numeric)
  
  output$parcoords <- renderPlotly({
    dims <- Map(function(x, y) {
      list(values = x, range = range(x), label = y)
    }, iris_numeric, names(iris_numeric), USE.NAMES = FALSE)
    plot_ly(type = 'parcoords', dimensions = dims, source = "pcoords") %>% 
      layout(margin = list(r = 30)) %>%
      event_register("plotly_restyle")
  })
  
  # maintain a collection of selection ranges
  # since each parcoord dimension is allowed to have multiple 
  # selected ranges, this reactive values data structure is 
  # allowed 
  # list(
  #  var1 = list(c(min1, max1), c(min2, max2), ...),
  #  var2 = list(c(min1, max1)),
  #  ...
  # )
  ranges <- reactiveValues()
  observeEvent(event_data("plotly_restyle", source = "pcoords"), {
    d <- event_data("plotly_restyle", source = "pcoords")
    # what is the relevant dimension (i.e. variable)?
    dimension <- as.numeric(stringr::str_extract(names(d[[1]]), "[0-9]+"))
    # If the restyle isn't related to a dimension, exit early.
    if (!length(dimension)) return()
    # careful of the indexing in JS (0) versus R (1)!
    dimension_name <- names(iris_numeric)[[dimension + 1]]
    # a given dimension can have multiple selected ranges
    # these will come in as 3D arrays, but a list of vectors 
    # is nicer to work with
    info <- d[[1]][[1]]
    
    ranges[[dimension_name]] <- if (length(dim(info)) == 3) {
      lapply(seq_len(dim(info)[2]), function(i) info[,i,])
    } else {
      list(as.numeric(info))
    }
  })
  
  ## filter the dataset down to the rows that match the selection ranges
  iris_selected <- reactive({
    keep <- TRUE
    for (i in names(ranges)) {
      range_ <- ranges[[i]]
      keep_var <- FALSE
      for (j in seq_along(range_)) {
        rng <- range_[[j]]
        keep_var <- keep_var | dplyr::between(iris[[i]], min(rng), max(rng))
      }
      keep <- keep & keep_var
    }
    iris[keep, ]
  })
  
  output$data <- renderPrint({
    tibble::as_tibble(iris_selected())
  })
}

shinyApp(ui, server)
