\name{signup}
\alias{signup}
\title{Sign up to plotly.}
\usage{
signup(username, email)
}
\arguments{
  \item{username}{desired username}

  \item{email}{desired email}
}
\value{
\itemize{ \item{api_key }{key to use with the api}
\item{tmp_pw }{temporary password to access your plotly
account} }
}
\description{
A sign up interface to Plotly through the R Console. See
documentation and examples at https://plot.ly/API}
}
\details{
See documentation and examples at https://plot.ly/API
}
\note{
https://plot.ly/API
}
\examples{
\dontrun{
# See up-to-date documentation and examples at https://plot.ly/API
username <- 'anna.lyst'
email <- 'anna.lyst@plot.ly'
response <- signup(username, email)
response$api_key # key to access plotly with
response$tmp_pw # temporary password to access your plotly account
}
}
\author{
Chris Parmer chris@plot.ly
}
\references{
https://plot.ly/API
}

