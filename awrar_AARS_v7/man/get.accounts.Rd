\name{get.accounts}
\alias{get.accounts}
\title{Get the water accounts from simulations produced by \link{awrar.run} and \link{irrigation.run}}
\usage{
  get.accounts(river.run, irrig.run, start,
    start.water.year = "07-01")
}
\arguments{
  \item{river.run}{A run obtained from \link{awrar.run}}

  \item{irrig.run}{A run obtained from
  \link{irrigation.run}}

  \item{start}{Start date of the simulation with format
  MM-DD}

  \item{start.water.year}{Start of the water year}
}
\value{
  a data frame containing the accounts
}
\description{
  The accounts are produced based on the template
  \link{accounts.template}.
}
\examples{
# Awrar run
inputs <- test.data1$inputs
config <- get.default.config()
dt <- config[7]
config[1] <- 1
config[2] <- 1
config[11] <- 0.6 # Second parameter of the Area/Volume relationship
parameters <- unlist(test.data1$parameters)
parameters[1] <- 1/50 				# Return flow coefficient
parameters[5] <- 50 					# Overbank flow threshold
river.run <- awrar.run(parameters,config,inputs)

# dummy irrigation data set
nval <- length(river.run$outflow)
zero <- rep(0,nval)
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
	irrig.run <- data2irrigation.run(data)

 accounts <- get.accounts(river.run,irrig.run,start="1970-01-01")
}

