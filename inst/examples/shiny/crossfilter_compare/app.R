library(shiny)
library(dplyr)
library(nycflights13)
library(colourpicker)
# install from https://github.com/hadley/ggstat
library(ggstat)

# Select all the (numeric) variables to be shown as histograms
# in the crossfilter
d <- select(flights, arr_time, dep_time, arr_delay, dep_delay, air_time, distance)

# Determing a color palette for the brush
n_colors <- 5
color_codes <- RColorBrewer::brewer.pal(n_colors, "Dark2")

# Generate an output container per variable
ui <- fluidPage(
  fluidRow(
    tags$div(
      style = "display:inline-block; max-width:150px; margin-left:50px",
      colourInput("brush_color", "Pick a brush color", color_codes[1], palette = "limited", allowedCols = color_codes)
    ),
    tags$div(
      style = "display:inline-block",
      actionButton("clear", "Clear Selection")
    )
  ),
  lapply(names(d), function(nm) plotlyOutput(nm, height = 200, width = "49%", inline = TRUE))
)

server <- function(input, output, session) {
  
  # Implement same render logic for each variable
  lapply(names(d), function(nm) {
    
    # By letting R handle the binning, there is less data to send over the
    # wire (and, thus, a more responsive app over back connections)
    counts <- d[[nm]] %>%
      bin_fixed(bins = 150) %>%
      compute_stat(d[[nm]]) %>%
      filter(!is.na(xmin_)) %>%
      mutate(
        xmid = (xmin_ + xmax_) / 2,
        prop_ = count_ / sum(count_), 
        zeros = 0
      )
    
    output[[nm]] <- renderPlotly({
      
      # Draw two layers of bars, one for the overall (black)
      # distribution, and one for the filtered data (red)
      p <- plot_ly(
        counts, x = ~xmid, alpha = I(0.5), 
        source = nm, unselected = list(marker = list(opacity = 1))
      ) %>%
        add_bars(y = ~prop_, color = I("black")) %>%
        layout(
          dragmode = "select", 
          selectdirection = "h",
          xaxis = list(
            title = nm,
            range = range(d[[nm]], na.rm = TRUE) 
          ),
          yaxis = list(
            title = "",
            showticks = FALSE, 
            showticklabels = FALSE, 
            showgrid = FALSE
          ),
          barmode = "overlay",
          showlegend = FALSE
        )
      
      for (col in color_codes) {
        p <- add_bars(p, y = ~zeros, color = I(col))
      }
      
      p
    })
    
    # when the selection is cleared, return the selection layer bars to 0
    observeEvent(input$clear, {
      plotlyProxy(nm, session) %>%
        plotlyProxyInvoke("restyle", "y", list(counts$zeros), seq_along(color_codes))
    })
    
    # each brush color code corresponds to a different trace index
    trace_index <- reactive(match(input$brush_color, color_codes))
    
    observe({
      b <- event_data("plotly_brushing", source = nm)$x
      in_bounds <- between(d[[nm]], min(b), max(b))
      
      lapply(names(d), function(var) {
        p <- plotlyProxy(var, session)
        
        if (is.null(b)) {
          
          # brush has been cleared, return the selection bars to a zero height
          plotlyProxyInvoke(p, "restyle", "y", list(counts$zeros), trace_index())
          
        } else {
          
          # if the brush originates from the proxy target
          # then don't compute a new marginal distribution, 
          # just highlight the range of interest
          props <- if (nm == var) {
            if_else(
              between(counts$xmin_, min(b), max(b)) & 
                between(counts$xmax_, min(b), max(b)),
              counts$prop_,
              0
            )
          } else {
            d[[var]] %>%
              bin_fixed(bins = 150) %>%
              compute_stat(d[[var]][in_bounds]) %>%
              filter(!is.na(xmin_)) %>%
              mutate(prop_ = count_ / sum(count_)) %>%
              pull(prop_)
          }
          
          plotlyProxyInvoke(p, "restyle", "y", list(props), trace_index())
        }
      })
      
    })
    
  })
  
}

shinyApp(ui, server)
