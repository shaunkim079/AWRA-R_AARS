\name{gr4j.run}
\alias{gr4j.run}
\title{Run the GR4J model}
\usage{
  gr4j.run(parameters, data, istart = 1, iend = nval,
    stateini = NULL)
}
\arguments{
  \item{parameters}{Vector of model parameters. The vector
  components should be named \itemize{ \item
  names(parameters)[1] : "S" (Production store) \item
  names(parameters)[2] : "IGF" (Groundwater exchange) \item
  names(parameters)[3] : "R" (Routing store) \item
  names(parameters)[4] : "TB" (Time base of unit
  hydrograph) }}

  \item{data}{a list object with the following structure
  \itemize{ \item P : Timeseries of rainfall (mm/time step,
  e.g mm/day for daily rainfall) \item PE : Timeseries of
  potential evapotranspiration (mm/time step) \item config
  : A list object containing the following elements
  \itemize{ \item dt : Time step length in seconds (default
  is 86400 sec) \item area : Catchment area in km2 (default
  is dt/1e3 so that the model produces a value in
  mm/timestep) }

  }}

  \item{istart}{Index of simulation start. Default is 1}

  \item{iend}{Index of simulation end. Default is the
  number of time steps in data}

  \item{stateini}{Initial filling level for the reservoirs}
}
\value{
  A list object having the following structure \itemize{
  \item output : Vector of calculated streamflow (m3/s).
  Warning ! This is NOT runoff.  \item states : Matrix of
  model states }
}
\description{
  Run the GR4J model
}
\examples{
# Get data from first example
data <- rr.data01
d <- list(P=data.frame(data$data[,2]),PE=data.frame(data$data[,3]),config=list(idmod=1))

# get parameters from first example
p <- data$params.gr4j
names(p) <- c("S","IGF","R","TB")

# Run model
out <- gr4j.run(p,d)

# plot results
plot(out$output,type="l",ylab="Simulated flow (m3/s)")
}

