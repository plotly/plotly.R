library(shiny)
library(plotly)
library(dplyr)
library(sparklyr)
library(dbplot)
sc <- spark_connect(master = "local", version = "2.1.0")
spark_flights <- copy_to(sc, nycflights13::flights, "flights")

# Collect numeric variables to be shown as histograms in the crossfilter
d <- select(
  spark_flights, 
  arr_time, 
  dep_time, 
  arr_delay, 
  dep_delay, 
  air_time, 
  distance
)

# Generate an output container per variable
ui <- fluidPage(
  lapply(colnames(d), function(nm) plotlyOutput(nm, height = 200, width = "49%", inline = TRUE))
)

server <- function(input, output, session) {
  
  # These reactive values track the set of active brushes 
  # Each reactive value corresponds to a different variable 
  brush_ranges <- reactiveValues()
  
  # Filter the dataset based on every active brush range except for one (var)
  # TODO: instead of filtering the entire dataset, 
  # consider filtering a pre-binned dataset based on pixel resolution
  # (perhaps using dbplot::bin_plot)
  d_filter <- function(d, var = "arr_time") {
    for (nm in setdiff(names(d), var)) {
      rng <- brush_ranges[[nm]]
      if (is.null(rng)) next
      d <- filter(d, between(d[[nm]], min(rng), max(rng)))
    }
    d
  }
  
  # Implement same render logic for each variable
  lapply(colnames(d), function(nm) {
    
    sym <- as.symbol(nm)
    counts_full <- db_compute_bins(spark_flights, !!sym)
    
    output[[nm]] <- renderPlotly({
      
      plot_ly(source = nm) %>%
        add_bars(x = counts_full[[nm]], y = ~counts_full$count) %>%
        layout(
          dragmode = "select", 
          selectdirection = "h",
          xaxis = list(
            title = nm,
            range = range(counts_full[[nm]], na.rm = TRUE)
          ),
          yaxis = list(title = "")
        )
    })
    
    observeEvent(event_data("plotly_brushing", source = nm), ignoreNULL = FALSE, {
      
      # inform the world about the new brush range
      brush_ranges[[nm]] <- event_data("plotly_brushing", source = nm)$x
      
      if (all(sapply(brush_ranges, function(x) length(x) == 0))) return()
      
      # update the bar heights of every view (except for the one being brushed)
      for (var in setdiff(names(d), nm)) {
        # views respect every brush except for their own 
        d_filtered <- d_filter(d, var)
        
        # bin the filtered data based on the global binning definition
        sym <- as.symbol(nm)
        counts_filter <- db_compute_bins(
          d_filtered, !!sym,
          binwidth = diff(counts_full[[nm]][1:2])
        )
        
        print(counts_filter)
        # finally, update the bar heights
        plotlyProxy(var, session) %>%
          plotlyProxyInvoke("restyle", "y", list(counts_filter$count), 0) 
      }
    })
    
  })
  
}

shinyApp(ui, server)

