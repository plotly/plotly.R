library(shiny)
library(plotly)
data(diamonds, package = "ggplot2")
nms <- names(diamonds)

shinyUI(pageWithSidebar(
	
	headerPanel("Diamonds Explorer"),
	
	sidebarPanel(
		
		sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(diamonds),
								value = 1000, step = 500, round = 0),
		
		selectInput('x', 'X', choices = nms, selected = "carat"),
		selectInput('y', 'Y', choices = nms, selected = "price"),
		selectInput('color', 'Color', choices = c('None', nms), selected = "clarity"),
		
		selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "clarity"),
		selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
		sliderInput('plotHeight', 'Height of plot (in pixels)', 
		            min = 100, max = 2000, value = 1000)
	),
	
	mainPanel(
	  plotlyOutput('trendPlot', height = "800px")
	)
))
