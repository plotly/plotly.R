% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly.R
\name{plot_dendro}
\alias{plot_dendro}
\title{Plot an interactive dendrogram}
\usage{
plot_dendro(d, set = "A", xmin = -50, height = 500, width = 500, ...)
}
\arguments{
\item{d}{a dendrogram object}

\item{set}{defines a crosstalk group}

\item{xmin}{minimum of the range of the x-scale}

\item{height}{height}

\item{width}{width}

\item{...}{arguments supplied to \code{\link[=subplot]{subplot()}}}
}
\description{
This function takes advantage of nested key selections to implement an
interactive dendrogram. Selecting a node selects all the labels (i.e. leafs)
under that node.
}
\examples{
\dontshow{if (interactive() || !identical(.Platform$OS.type, "windows")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

\dontrun{
hc <- hclust(dist(USArrests), "ave")
dend1 <- as.dendrogram(hc)
plot_dendro(dend1, height = 600) \%>\% 
  hide_legend() \%>\% 
  highlight(persistent = TRUE, dynamic = TRUE)
}
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=plot_ly]{plot_ly()}}, \code{\link[=plot_mapbox]{plot_mapbox()}}, \code{\link[=ggplotly]{ggplotly()}}
}
\author{
Carson Sievert
}
