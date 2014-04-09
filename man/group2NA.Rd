\name{group2NA}
\alias{group2NA}
\title{Drawing ggplot2 geoms with a group aesthetic is most efficient in
plotly when we convert groups of things that look the same to
vectors with NA.}
\usage{
group2NA(g, geom)
}
\arguments{
  \item{g}{list of geom info with g$data$group.}

  \item{geom}{change g$geom to this.}
}
\value{
list of geom info.
}
\description{
Drawing ggplot2 geoms with a group aesthetic is most
efficient in plotly when we convert groups of things that
look the same to vectors with NA.
}
\author{
Toby Dylan Hocking
}

