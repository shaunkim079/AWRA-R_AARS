\name{get.actual.x}
\alias{get.actual.x}
\title{Get the actual x Muskingum parameter for given K and dt}
\usage{
  get.actual.x(x, K, dt)
}
\arguments{
  \item{x}{Proposed x value}

  \item{K}{Value of the K parameter (seconds)}

  \item{dt}{Time step duration (seconds)}
}
\value{
  actual x value avoiding negative outflows
}
\description{
  The actual x value is determined as actual.x =
  min(dt/2/k,max(1-dt/2/k))
}
\examples{
###
}

