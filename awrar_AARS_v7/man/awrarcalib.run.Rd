\name{awrarcalib.run}
\alias{awrarcalib.run}
\title{Routine used to calibrate awrar}
\usage{
  awrarcalib.run(parameters, data, istart = 1, iend = nval,
    stateini = NULL)
}
\arguments{
  \item{parameters}{Vector of model parameters (see
  \link{awrar.run}) \itemize{ \item p[1]: Flood return
  \item p[2]: GW monod 1 \item p[3]: GW monod 2 \item p[4]:
  Overbank flow thresh \item p[5]: Overbank flow exponent
  \item p[6]: Inverse velocity (s/m) \item p[7]: diffusion
  coefficient (-) \item p[8]: RR par 1 (e.g. scaling
  factor) \item p[9]: RR par 2 \item p[10]: RR par 3 \item
  p[11]: RR par 4 }}

  \item{data}{Input data (see ) \itemize{ \item inputs :
  inputs to \link{awrar.run} function \item P : rainfall to
  be used by GR4J \item PE: APET to be used by GR4J \item
  config : config for \link{awrar.run} function + \itemize{
  \item config[16] : Flood Ksat \item config[17] : River
  length(m) } }}

  \item{istart}{Index of simulation start}

  \item{iend}{Index of simulation end}

  \item{stateini}{Initial filling level for the reservoirs
  of sacramento}
}
\value{
  A list object (nothing at the moment)
}
\description{
  The model is based on the C core of AWRA-R used in
  \link{awrar.run} with fixed Ksat.
}

