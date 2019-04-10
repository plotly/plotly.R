library(shiny)
library(plotly)
library(dplyr)

data(sales, package = "plotlyBook")
categories <- unique(sales$category)
sub_categories <- unique(sales$sub_category)
ids <- unique(sales$id)

ui <- fluidPage(
  uiOutput("history"),
  plotlyOutput("bars", height = 200),
  plotlyOutput("lines", height = 300)
)

server <- function(input, output, session) {
  # These reactive values keep track of the drilldown state
  # (NULL means inactive)
  drills <- reactiveValues(
    category = NULL,
    sub_category = NULL,
    id = NULL
  )
  # filter the data based on active drill-downs
  # also create a column, value, which keeps track of which
  # variable we're interested in 
  sales_data <- reactive({
    if (!length(drills$category)) {
      return(mutate(sales, value = category))
    }
    sales <- filter(sales, category %in% drills$category)
    if (!length(drills$sub_category)) {
      return(mutate(sales, value = sub_category))
    }
    sales <- filter(sales, sub_category %in% drills$sub_category)
    mutate(sales, value = id)
  })
  
  # bar chart of sales by 'current level of category'
  output$bars <- renderPlotly({
    d <- count(sales_data(), value, wt = sales)
    
    p <- plot_ly(d, x = ~value, y = ~n, source = "bars") %>%
      layout(
        yaxis = list(title = "Total Sales"), 
        xaxis = list(title = "")
      )
    
    if (!length(drills$sub_category)) {
      add_bars(p, color = ~value)
    } else if (!length(drills$id)) {
      add_bars(p) %>%
        layout(
          hovermode = "x",
          xaxis = list(showticklabels = FALSE)
        )
    } else {
      # add a visual cue of which ID is selected
      add_bars(p) %>%
        filter(value %in% drills$id) %>%
        add_bars(color = I("black")) %>%
        layout(
          hovermode = "x", xaxis = list(showticklabels = FALSE),
          showlegend = FALSE, barmode = "overlay"
        )
    }
  })
  
  # time-series chart of the sales
  output$lines <- renderPlotly({
    p <- if (!length(drills$sub_category)) {
      sales_data() %>%
        count(order_date, value, wt = sales) %>%
        plot_ly(x = ~order_date, y = ~n) %>%
        add_lines(color = ~value)
    } else if (!length(drills$id)) {
      sales_data() %>%
        count(order_date, wt = sales) %>%
        plot_ly(x = ~order_date, y = ~n) %>%
        add_lines()
    } else {
      sales_data() %>%
        filter(id %in% drills$id) %>%
        select(-value) %>%
        plot_ly() %>% 
        add_table()
    }
    p %>%
      layout(
        yaxis = list(title = "Total Sales"), 
        xaxis = list(title = "")
      )
  })
  
  # control the state of the drilldown by clicking the bar graph
  observeEvent(event_data("plotly_click", source = "bars"), {
    x <- event_data("plotly_click", source = "bars")$x
    if (!length(x)) return()
    
    if (!length(drills$category)) {
      drills$category <- x
    } else if (!length(drills$sub_category)) {
      drills$sub_category <- x
    } else {
      drills$id <- x
    }
  })
  
  # populate a `selectInput()` for each active drilldown 
  output$history <- renderUI({
    if (!length(drills$category)) return("Click the bar chart to drilldown")
    categoryInput <- selectInput(
      "category", "Category", 
      choices = categories, selected = drills$category
    )
    if (!length(drills$sub_category)) return(categoryInput)
    sd <- filter(sales, category %in% drills$category)
    subCategoryInput <- selectInput(
      "sub_category", "Sub-category", 
      choices = unique(sd$sub_category), 
      selected = drills$sub_category
    )
    if (!length(drills$id)) {
      return(fluidRow(
        column(3, categoryInput), 
        column(3, subCategoryInput)
      ))
    }
    sd <- filter(sd, sub_category %in% drills$sub_category)
    idInput <- selectInput(
      "id", "Product ID", 
      choices = unique(sd$id), selected = drills$id
    )
    fluidRow(
      column(3, categoryInput), 
      column(3, subCategoryInput),
      column(3, idInput)
    )
  })
  
  # control the state of the drilldown via the `selectInput()`s
  observeEvent(input$category, {
    drills$category <- input$category
    drills$sub_category <- NULL
    drills$id <- NULL
  })
  observeEvent(input$sub_category, {
    drills$sub_category <- input$sub_category
    drills$id <- NULL
  })
  observeEvent(input$id, {
    drills$id <- input$id
  })
}

shinyApp(ui, server)
