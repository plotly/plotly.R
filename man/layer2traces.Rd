\name{layer2traces}
\alias{layer2traces}
\title{Convert a layer to a list of traces. Called from gg2list()}
\usage{
layer2traces(l, d, is.continuous)
}
\arguments{
  \item{l}{one layer of the ggplot object}

  \item{d}{one layer of calculated data from
  ggplot2::ggplot_build(p)}

  \item{is.continuous}{logical vector.}
}
\value{
list representing a layer, with corresponding aesthetics,
ranges, and groups.
}
\description{
Convert a layer to a list of traces. Called from gg2list()
}

