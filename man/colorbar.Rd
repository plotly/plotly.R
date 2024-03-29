% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{colorbar}
\alias{colorbar}
\title{Modify the colorbar}
\usage{
colorbar(p, ..., limits = NULL, which = 1)
}
\arguments{
\item{p}{a plotly object}

\item{...}{arguments are documented here
\url{https://plotly.com/r/reference/#scatter-marker-colorbar}.}

\item{limits}{numeric vector of length 2. Set the extent of the colorbar scale.}

\item{which}{colorbar to modify? Should only be relevant for subplots with
multiple colorbars.}
}
\description{
Modify the colorbar
}
\examples{
\dontshow{if (interactive() || !identical(.Platform$OS.type, "windows")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

p <- plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~cyl)

# pass any colorbar attribute -- 
# https://plotly.com/r/reference/#scatter-marker-colorbar
colorbar(p, len = 0.5)

# Expand the limits of the colorbar
colorbar(p, limits = c(0, 20))
# values outside the colorbar limits are considered "missing"
colorbar(p, limits = c(5, 6))

# also works on colorbars generated via a z value
corr <- cor(diamonds[vapply(diamonds, is.numeric, logical(1))])
plot_ly(x = rownames(corr), y = colnames(corr), z = corr) \%>\%
 add_heatmap() \%>\%
 colorbar(limits = c(-1, 1))
\dontshow{\}) # examplesIf}
}
\author{
Carson Sievert
}
