\name{write.csv.header}
\alias{write.csv.header}
\title{Write a csv file with a header}
\usage{
  write.csv.header(data, filename, header = NULL,
    author = Sys.getenv("USER"), source.script = "unknown",
    ...)
}
\arguments{
  \item{data}{Data frame to be printed}

  \item{filename}{Output file name}

  \item{header}{vector of strings containing headerer
  information. header can be null, but comment(data) has to
  be filled up. This is to force user to write comments
  about its data.}

  \item{author}{File author (default is USER environment
  variable)}

  \item{source.script}{File author (default is USER
  environment variable)}

  \item{...}{Other arguments passed to write.table}
}
\value{
  NULL
}
\description{
  Wrapper around the \link{write.table} function to produce
  a csv file with a header. The header contains comments
  added to the data via the \link{comment} function and/or
  comments passed to the current function via the
  \code{header} argument.
}
\examples{
d <- data.frame(matrix(rnorm(100),20,5))
comment(d) <- c("this is a header 1","this is a header 2","etc...")
ff <- "~/d.csv"
author <- "Me"
write.csv.header(d,file=ff,author=author)

# the mention 'comment.char' has to be used when reading the data
dd <- read.csv(ff,comment.char="#")
}

