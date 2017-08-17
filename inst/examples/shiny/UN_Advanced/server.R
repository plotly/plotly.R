#server script for United Nations Advanced Example
library(shiny)
library(plotly)
library(dplyr)

shinyServer(function(input, output, session) {

  output$trendPlot <- renderPlotly({
    if (length(input$name) == 0) {
      print("Please select at least one country")
    } else {
      df_trend <- ideal[ideal$Name == input$name, ]

      # Graph title
      if (length(input$name) > 2) {
        j_names_comma <- paste(input$name[-length(input$name)], collapse = ', ')
        j_names <- paste0(j_names_comma, ", and ", input$name[length(input$name)])
      } else {
        j_names <- paste(input$name, collapse = ' and ')
      }

      graph_title  <- paste("Ideal Points for ", j_names, sep="")

      ggideal_point <- ggplot(df_trend) +
        geom_line(aes(x = Year, y = Ideal.point, color = Name)) +
        labs(x = "Year", y = "Ideology", title = graph_title) +
        scale_colour_hue("clarity", l = 70, c = 150) + ggthemes::theme_few() +
        theme(legend.position = "none")

      # Convert ggplot object to plotly
      ggplotly(ggideal_point) %>%
        layout(
          showlegend = FALSE,
          # increase the size of the right margin to accommodate more room for the annotation labels
          margin = list(r = 170)
        )
    }
  })

  output$termPlot <- renderPlot({
    df_term <- ideal  %>% filter(Name %in% input$name) %>%
      group_by(Name) %>% summarise(terms = n())

    trans_theme <- theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(fill = NA)
    )

    ggplot(df_term, aes(x = reorder(Name, terms), y = terms))+
      geom_bar(stat = "identity", fill = "#2980b9") + coord_flip() +
      theme_bw() + trans_theme + labs(y = "Terms (in years)", x = "")
    
  }, bg="transparent")
})
