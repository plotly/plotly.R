library(shiny)
library(plotly)
data(diamonds, package = "ggplot2")

shinyServer(function(input, output, session) {

	#add reactive data information. Dataset = built in diamonds data
	dataset <- reactive({
	  diamonds[sample(nrow(diamonds), input$sampleSize),]
	})
	
	output$trendPlot <- renderPlotly({
		# build graph with ggplot syntax
		p <- ggplot(dataset(), aes_string(x = input$x, y = input$y)) +
		  geom_point()
    
		# if color is specified, add it as an aesthetic
		if (input$color != 'None') p <- p + aes_string(color=input$color)
    
		# if at least one facet column/row is specified, add it
		facets <- paste(input$facet_row, '~', input$facet_col)
		if (facets != '. ~ .') p <- p + facet_grid(facets)
    # return the ggplot object and renderPlotly() will know how to handle it
		toWebGL(p)
	})

})
