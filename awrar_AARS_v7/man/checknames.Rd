\name{checknames}
\alias{checknames}
\title{Check the name of an object attributes}
\usage{
  checknames(obj, expected.names, undesirable.names = NULL,
    objname = "")
}
\arguments{
  \item{obj}{A S3 list object}

  \item{expected.names}{Vector of strings listing the names
  of the attributes or arguments that should be present in
  \code{obj}}

  \item{undesirable.names}{Vector of strings listing the
  names of the attributes or arguments that should not be
  present in \code{obj}}

  \item{objname}{Object name to be mentioned in the output
  message}
}
\value{
  A list object containing two elements: \code{passed}
  indicates the test is passed, \code{message} gives some
  info
}
\description{
  Check the name of an object attributes
}
\examples{
o <- list(u="AADSASD",v=rnorm(100))
ck1 <- checknames(o,c("u","v"))
ck2 <- checknames(o,c("u","w"))
}

