\name{subperiods}
\alias{subperiods}
\title{Generate subperiods for a split-sample test exercise}
\usage{
  subperiods(nval, nper, warmup, validata,
    addfullperiod = FALSE)
}
\arguments{
  \item{nval}{Number of time step}

  \item{nper}{Number of subperiods}

  \item{warmup}{Duration of the warmup period at the
  beginning of each sub-period}

  \item{validata}{Indices of the vector where an objective
  function or a performance score can be computed (e.g. non
  missing values).}

  \item{addfullperiod}{Add a subperiod covering the whole
  period. Default is FALSE.}
}
\value{
  A list object containing \itemize{ \item startend : A
  matrix containing the index of start and end of
  subperiods \item pervalidata : A list object containing
  the indices of the valid data in each period (taking into
  account the warmup period) }
}
\description{
  Generate subperiods for a split-sample test exercise
}
\examples{
nval <- 1000
s<-subperiods(nval,3,100,1:nval)
s$startend
}

