all: build
	R --interactive < ~/Documents/plotly/wip_missing_bars.R

build:
	R CMD build /Users/pedro/Documents/plotly/ropensci/plotly/
	R CMD INSTALL --library=/Library/Frameworks/R.framework/Versions/3.1/scratch plotly_0.5.1.tar.gz
