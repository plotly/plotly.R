\name{layer2list}
\alias{layer2list}
\title{Convert a layer to a list. Called from gg2list()}
\usage{
layer2list(l, d, ranges)
}
\arguments{
  \item{l}{one layer of the ggplot object}

  \item{d}{one layer of calculated data from
  ggplot2::ggplot_build(p)}

  \item{ranges}{axes ranges}
}
\value{
list representing a layer, with corresponding aesthetics,
ranges, and groups.
}
\description{
Convert a layer to a list. Called from gg2list()
}

