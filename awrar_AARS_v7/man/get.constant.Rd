\name{get.constant}
\alias{get.constant}
\title{Get a constant declared in the C header file}
\usage{
  get.constant(constant.name)
}
\arguments{
  \item{constant.name}{Name of constant}
}
\value{
  constant value. NA if not found in the list of constants.
}
\description{
  The constants are used to define the dimensions of some
  vectors and arrays.
}
\examples{
get.constant("NPARROUTING")
get.constant("NPARROUTIN")
}

