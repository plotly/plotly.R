\name{signup}
\alias{signup}
\title{Sign up to plotly}
\description{A sign up interface to Plotly through the R Console. See documentation and examples at https://plot.ly/API}
\usage{signup(username, email)}
\arguments{
  \item{username}{
 desired username
}
  \item{email}{
   desired email
}
}
\details{
 See documentation and examples at https://plot.ly/API
}
\value{
  \item{api_key }{key to use with the api}
  \item{tmp_pw }{temporary password to access your plotly account}
}
\references{
 https://plot.ly/API
}
\author{
  <chris@plot.ly>
}
\note{
  https://plot.ly/API
}

\seealso{
}
\examples{
# See up-to-date documentation and examples at https://plot.ly/API
# username <- 'anna.lyst'
# email <- 'anna.lyst@plot.ly'
# response <- signup(username, email)
# response$api_key # key to access plotly with
# response$tmp_pw # temporary password to access your plotly account
}