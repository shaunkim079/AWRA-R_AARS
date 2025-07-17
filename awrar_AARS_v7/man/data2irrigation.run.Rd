\name{data2irrigation.run}
\alias{data2irrigation.run}
\title{Convert irrigation data to the \link{irrigation.run} format}
\usage{
  data2irrigation.run(data)
}
\arguments{
  \item{data}{Data frame containing irrigation model
  outputs with the following column names : \itemize{ \item
  ofs \item swdiversion \item gwdiversion \item
  floodharvest \item returnflow \item evapofs \item
  rainfallofs \item evapcrop \item rainfallcrop \item
  applicationcrop \item gwlossofs }}
}
\value{
  A list object with the structure detailed in
  \link{irrigation.run}
}
\description{
  The conversion is required to compute the water accounts
  with the \link{get.accounts} function.
}
\examples{
# dummy data set
zero <- rep(0,5000)
data <- data.frame(ofs=zero,
			swdiversion=zero,
			gwdiversion=zero,
			floodharvest=zero,
			returnflow=zero,
			evapofs=zero,
			rainfallofs=zero,
			evapcrop=zero,
			rainfallcrop=zero,
			applicationcrop=zero,
			gwlossofs=zero)

	out <- data2irrigation.run(data)
}

