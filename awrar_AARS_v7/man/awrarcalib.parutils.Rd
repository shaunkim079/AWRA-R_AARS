\name{awrarcalib.parutils}
\alias{awrarcalib.parutils}
\title{Return utility functions and data to deal with the parameters of the awrar model}
\usage{
  awrarcalib.parutils(config = NULL)
}
\arguments{
  \item{config}{Configuration object}
}
\value{
  a list object containing \itemize{ \item inputnames :
  names of the \code{M} inputs \item parnames : names of
  the \code{N} parameters \item checkmodpar : function to
  check that a parameter sets has the correct length and
  names \item parlib : a matrix \code{P}x\code{N} of
  parameter containing an ensemble of \code{P} plausible
  parameter sets \item parbounds : a matrix \code{N}x2
  containing the lower and upper boundary of the parameter
  \item true2trans : a function returning the transformed
  values of a parameter set (for optimisation) \item
  trans2true : a function returning the real values of a
  transformed parameter set (for optimisation) }
}
\description{
  Return utility functions and data to deal with the
  parameters of the awrar model
}
\examples{
pu <- awrarcalib.parutils()
}

