\name{plotly}
\alias{plotly}
\title{
 main interface to plotly
}
\description{
plotly interface object. See up-to-date documentation and examples at https://plot.ly/API
}
\usage{
# py <- plotly(username, key)
# py has 3 methods:
# 1 - Plotting: py$plotly(x1, y1[,x2,y2,...], kwargs=kw) or py$plotly({data1[,data2,...]}, kwargs=kwargs) 
# 2 - Styling Data: py$style(data1,data2,..., kwargs=kwargs)
# 3 - Styling Layout: py$layout(layout, kwargs=kwargs)
}
\arguments{
  \item{username}{
   plotly username
}
  \item{key}{
     plotly API key
}
}
\details{
See documentation and examples at https://plot.ly/API
}
\value{

}
\references{
 https://plot.ly/API
}
\author{
  <chris@plot.ly>
}
\note{
}

\seealso{
}
\examples{
## View https://plot.ly/API for more examples
## Generate a simple plot 
# username <- "anna.lyst" # fill in with your plotly username
# api_key <- "y37zkd" # fill in with your plotly API key
# py <- plotly(username, api_key)
## generate some data
# x <- c(0,1,2)
# y <- c(10,11,12)
## send data to Plotly. Plotly will render an interactive graph and will return a URL where you can view your plot
# response <- py$plot(x,y) # sends data to Plotly, Plotly renders an interactive graph, and returns a URL where you can view your plot
# url = response$url # view your plot at this URL
}
