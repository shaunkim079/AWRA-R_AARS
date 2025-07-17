\name{flood}
\alias{flood}
\title{Flood routine used to test the AWRAR flood component}
\usage{
  flood(Qin, P, E, paramj, floodAlpha, Ksat)
}
\arguments{
  \item{Qin}{Floodplain inflow}

  \item{P}{Floodplain rainfall}

  \item{E}{Floodplain evaporation}

  \item{paramj}{Floodplain model parameters \itemize{ \item
  paramj[1] : Overbank flow threshold \item paramj[2] :
  multiplier in overbank flow model \item paramj[3] :
  exponent in overbank flow model }}

  \item{floodAlpha}{Multiplier in the relationship flooded
  area = a . flooded volume}

  \item{Ksat}{Floodplain infiltration}
}
\value{
  A list object with the following structure: \itemize{
  \item outflow : Flow at the downstream end of the reach
  \item states.nonrouting : model states not related to
  routing (see awrar_runtimestep.c) \item states.routing :
  model states related to routing (see awrar_runtimestep.c)
  }
}
\description{
  Routine coded by Justin Hughes in Fortran
}
\examples{
## Inputs
	Qin <- c(0,100,rep(0,4))
	n <- length(Qin)
	P <- rexp(n)*1e-3/86400
	E <- (P*0+5)*1e-3/86400

	## parameters
	paramj <- c(90,1,0.5)
	floodAlpha <- 1/2
	Ksat <- 1e-2

	## model
	out <- flood(Qin,P,E,paramj,floodAlpha,Ksat)
	matplot(out,type="o")
}

