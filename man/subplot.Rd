% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subplots.R
\name{subplot}
\alias{subplot}
\title{View multiple plots in a single view}
\usage{
subplot(
  ...,
  nrows = 1,
  widths = NULL,
  heights = NULL,
  margin = 0.02,
  shareX = FALSE,
  shareY = FALSE,
  titleX = shareX,
  titleY = shareY,
  which_layout = "merge"
)
}
\arguments{
\item{...}{One of the following
\itemize{
\item any number of plotly/ggplot2 objects.
\item a list of plotly/ggplot2 objects.
\item a tibble with one list-column of plotly/ggplot2 objects.
}}

\item{nrows}{number of rows for laying out plots in a grid-like structure.
Only used if no domain is already specified.}

\item{widths}{relative width of each column on a 0-1 scale. By default all
columns have an equal relative width.}

\item{heights}{relative height of each row on a 0-1 scale. By default all
rows have an equal relative height.}

\item{margin}{either a single value or four values (all between 0 and 1).
If four values are provided, the first is used as the left margin, the second
is used as the right margin, the third is used as the top margin, and the
fourth is used as the bottom margin.
If a single value is provided, it will be used as all four margins.}

\item{shareX}{should the x-axis be shared amongst the subplots?}

\item{shareY}{should the y-axis be shared amongst the subplots?}

\item{titleX}{should x-axis titles be retained?}

\item{titleY}{should y-axis titles be retained?}

\item{which_layout}{adopt the layout of which plot? If the default value of
"merge" is used, layout options found later in the sequence of plots will
override options found earlier in the sequence. This argument also accepts a
numeric vector specifying which plots to consider when merging.}
}
\value{
A plotly object
}
\description{
View multiple plots in a single view
}
\examples{
\dontshow{if (interactive() || !identical(.Platform$OS.type, "windows")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

# pass any number of plotly objects to subplot()
p1 <- plot_ly(economics, x = ~date, y = ~uempmed)
p2 <- plot_ly(economics, x = ~date, y = ~unemploy)
subplot(p1, p2, p1, p2, nrows = 2, margin = 0.05)

#'  # anchor multiple traces on the same legend entry
 p1 <- add_lines(p1, color = I("black"), name = "1st", legendgroup = "1st")
 p2 <- add_lines(p2, color = I("red"), name = "2nd", legendgroup = "2nd")
 
 subplot(
   p1, style(p1, showlegend = FALSE),
   p2, style(p2, showlegend = FALSE),
   nrows = 2, margin = 0.05
 )

# or pass a list
economics_long \%>\%
  split(.$variable) \%>\%
  lapply(function(d) plot_ly(d, x = ~date, y = ~value)) \%>\%
  subplot(nrows = NROW(.), shareX = TRUE)
  
# or pass a tibble with a list-column of plotly objects
economics_long \%>\%
  group_by(variable) \%>\%
  do(p = plot_ly(., x = ~date, y = ~value)) \%>\%
  subplot(nrows = NROW(.), shareX = TRUE)
  
# learn more at https://plotly.com/r/subplots/
\dontshow{\}) # examplesIf}
}
\author{
Carson Sievert
}
