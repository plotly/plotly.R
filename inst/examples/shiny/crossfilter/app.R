library(shiny)
library(plotly)
library(dplyr)
library(nycflights13)
# install from https://github.com/hadley/ggstat
library(ggstat)

# Collect numeric variables to be shown as histograms in the crossfilter
d <- select(flights, arr_time, dep_time, arr_delay, dep_delay, air_time, distance)

# Generate an output container per variable
ui <- fluidPage(
  lapply(names(d), function(nm) plotlyOutput(nm, height = 200, width = "49%", inline = TRUE))
)

server <- function(input, output, session) {
  
  # These reactive values track the set of active brushes 
  # Each reactive value corresponds to a different variable 
  brush_ranges <- reactiveValues()
  
  # Filter the dataset based on every active brush range except for one (var)
  d_filter <- function(d, var = "arr_time") {
    for (nm in setdiff(names(d), var)) {
      rng <- brush_ranges[[nm]]
      if (is.null(rng)) next
      d <- filter(d, between(d[[nm]], min(rng), max(rng)))
    }
    d
  }
  
  # Implement same render logic for each variable
  lapply(names(d), function(nm) {
    
    counts <- d[[nm]] %>%
      bin_fixed(bins = 150) %>%
      compute_stat(d[[nm]]) %>%
      filter(!is.na(xmin_)) %>%
      mutate(xmid = (xmin_ + xmax_) / 2)
    
    output[[nm]] <- renderPlotly({
      
      plot_ly(counts, source = nm) %>%
        add_bars(x = ~xmid, y = ~count_) %>%
        layout(
          dragmode = "select", 
          selectdirection = "h",
          xaxis = list(
            title = nm,
            range = range(d[[nm]], na.rm = TRUE)
          ),
          yaxis = list(title = "")
        )
    })
    
    observeEvent(event_data("plotly_brushing", source = nm), ignoreNULL = FALSE, {
      
      # inform the world about the new brush range
      brush_ranges[[nm]] <- event_data("plotly_brushing", source = nm)$x
      
      # update the bar heights of every view (except for the one being brushed)
      for (var in setdiff(names(d), nm)) {
        
        # views respect every brush except for their own 
        d_filtered <- d_filter(d, var)
        
        # bin the filtered data based on the global binning definition
        counts_filter <- d[[var]] %>%
          bin_fixed(bins = 150) %>%
          compute_stat(d_filtered[[var]]) %>%
          filter(!is.na(xmin_)) %>%
          mutate(xmid = (xmin_ + xmax_) / 2)
        
        # finally, update the bar heights
        plotlyProxy(var, session) %>%
          plotlyProxyInvoke("restyle", "y", list(counts_filter$count_), 0) 
      }
    })
    
  })
  
}

shinyApp(ui, server)
