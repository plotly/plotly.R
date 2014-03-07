\name{plotly}
\alias{plotly}
\title{Main interface to plotly}
\usage{
plotly(username = NULL, key = NULL)
}
\arguments{
  \item{username}{plotly username}

  \item{key}{plotly API key}
}
\value{
An object of class PlotlyClass
}
\description{
A call to \code{plotly(username, key)} creates an object of
class "PlotlyClass", which has 3 methods: \itemize{ \item
Plotting: py$plotly(x1, y1[,x2,y2,...], kwargs=kw) or
py$plotly({data1[,data2,...]}, kwargs=kwargs) \item Styling
Data: py$style(data1,data2,..., kwargs=kwargs) \item
Styling Layout: py$layout(layout, kwargs=kwargs) }
}
\details{
Plotly interface object. See up-to-date documentation and
examples at https://plot.ly/API

See documentation and examples at https://plot.ly/API
}
\examples{
\dontrun{
## View https://plot.ly/API for more examples
## Generate a simple plot
username <- "anna.lyst" # fill in with your plotly username
api_key <- "y37zkd" # fill in with your plotly API key
py <- plotly(username, api_key)
## generate some data
x <- c(0,1,2)
y <- c(10,11,12)

## Send data to Plotly. Plotly will render an interactive graph and will return a
## URL where you can view your plot
## This call sends data to Plotly, Plotly renders an interactive
##    graph, and returns a URL where you can view your plot
response <- py$plot(x,y)
url = response$url # view your plot at this URL
browseURL(url)
}
}
\author{
Chris Parmer chris@plot.ly
}
\references{
https://plot.ly/API
}

