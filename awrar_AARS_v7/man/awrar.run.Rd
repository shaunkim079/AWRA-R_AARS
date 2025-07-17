\name{awrar.run}
\alias{awrar.run}
\title{Run awrar model}
\usage{
  awrar.run(parameters, config, inputs, states = NULL,
    use_irrig = 0, irrigParameters = NULL,
    irrigConfig = NULL, irrigInputs = NULL,
    irrigStates = NULL, is_headwater = 0)
}
\arguments{
  \item{parameters}{Model parameters ( vector \eqn{[3
  \times ninflow+4] \times 1}) with \itemize{ \item
  parameters[1] : flood return flow coefficient
  (dimensionless) \item parameters[2] : flood Ksat (m/s)
  \item parameters[3] : Monod river loss parameter a (m3/s)
  \item parameters[4] : Monod river loss parameter b (m3/s)
  \item parameters[5] : overbank flow threshold,
  \eqn{Q_o^*} (m3/s) \item parameters[6] : exponent used to
  compute overbankflow thresold, \eqn{\gamma<1} \item
  parameters[7] : runoff correction factor (-) \item
  parameters[8] : aquifer specific yield (-) \item
  parameters[9] : aquifer Ksat (m/s) \item parameters[10] :
  aquifer thickness (m) \item parameters[11] : surface
  layer thickness (m) \item parameters[12] : river
  conductance (m/s) \item parameters[13] : lag parameter
  \eqn{\delta} for inflow 1 (s) \item parameters[14] :
  Muskingum K for inflow 1 (s) \item parameters[15] :
  Muskingum x for inflow 1 (-) \item parameters[16] : lag
  parameter \eqn{\delta} for inflow 2 (s) \item
  parameters[17] : Muskingum K for inflow 2 (s) \item
  parameters[18] : Muskingum x for inflow 2 (-) \item ...
  same routing parameters (\eqn{\delta}, K and x) for other
  inflows }}

  \item{config}{Configuration data (vector \eqn{14 \times
  1}) with \itemize{ \item config[1] : use routing (=1) or
  not (=0) \item config[2] : use flood model (=1) or not
  (=0) \item config[3] : use monod model (=1) or not (=0)
  \item config[4] : use reservoir model (=1) or not (=0)
  \item config[5] : use ungauged inflows (=1) or not (=0)
  \item config[6] : use anabranch (=1) or not (=0) \item
  config[7] : time step duration (seconds) \item config[8]
  : river area/volume relationship, \eqn{\alpha} \item
  config[9] : river area/volume relationship, \eqn{\beta}
  \item config[10] : floodplain area/volume relationship,
  \eqn{\alpha} (not used at the moment) \item config[11] :
  floodplain area/volume relationship, \eqn{\beta} \item
  config[12] : anabranch loss - upstream of floodplain,
  parameter a \item config[13] : anabranch loss - upstream
  of floodplain, parameter b \item config[14] : anabranch
  loss - downstream of floodplain, parameter a \item
  config[15] : anabranch loss - downstream of floodplain,
  parameter b \item config[16] : floodplain length (m)
  \item config[17] : river depth/flow relationship,
  \eqn{\alpha} \item config[18] : river depth/flow
  relationship, \eqn{\beta} }}

  \item{inputs}{Input data (array \eqn{nval \times
  [11+ninflow]}) with \itemize{ \item col 1 : rainfall
  river (mm/time step) \item col 2 : evap river (mm/time
  step) \item col 3 : rainfall floodplain (mm/time step)
  \item col 4 : evap floodplain (mm/time step) \item col 5
  : irrigation diversion (m3/s) \item col 6 : irrigation
  return flow (m3/s) \item col 7 : diversion for urban
  water supply (m3/s) \item col 8 : ungauged inflow - top
  (m3/s) \item col 9 : ungauged inflow - bottom (m3/s)
  \item col 10 : reservoir volume (m3) \item col 11 :
  reservoir area (m2) \item col 12 : depth to groundwater
  (m) \item col 13 : river depth (m) \item col 14 : river
  width (m) \item col 15 : inflow 1 (m3/s) \item col 16 :
  inflow 2 (m3/s) \item col 17 : ... (other inflows) (m3/s)

  }}
}
\value{
  A list object with the following structure: \itemize{
  \item outflow : Flow at the downstream end of the reach
  \item states.nonrouting : model states not related to
  routing (see awrar_runtimestep.c): \itemize{ \item col 1:
  outflow from reach (m3/s) \item col 2: overbank_flow from
  river to floodplain (m3/s) \item col 3: floodplain volume
  (m3), \item col 4: floodplain area (m2), \item col 5:
  return flow from floodplain to river (m3/s) \item col 6:
  river rainfall flux (m3/s) \item col 7: river evap flux
  (m3/s) \item col 8: floodplain rainfall flux (m3/s) \item
  col 9: floodplain evap flux (m3/s) \item col 10:
  floodplain groundwater loss (m3/s) \item col 11: river
  groudwater loss (monod) (m3/s) \item col 12: Top
  anabranch loss (upstream of floodplain, m3/s) \item col
  13: Bottom anabranch loss (downstream of floodplain,
  m3/s) \item col 14: previous reservoir volume (m3) \item
  col 15: previous reservoir area (m2) \item col 16:
  rainfall flux on reservoir (m3/s) \item col 17: evap flux
  on reservoir (m3/s) \item col 18: reservoir contribution
  (m3/s) \item col 19: carryover reservoir flux (m3/s)
  \item col 20: outflow 2 (m3/s) \item col 21: outflow 3
  (m3/s) \item col 22: outflow 4 (m3/s) \item col 23:
  outflow 5 (m3/s) \item col 24: outflow 6 (m3/s) \item col
  25: outflow 7 (m3/s) \item col 26: river volume (m3)
  \item col 27: floodplain groundwater max change in
  storage (m3/s) \item col 28: floodplain groundwater
  outflow (m3/s) \item col 29: floodplain groundwater
  maximum infiltration (m3/s) \item col 30: river
  groundwater max change in storage (m3/s) \item col 31:
  river groundwater outflow (m3/s) \item col 32: river
  groundwater maximum infiltration (m3/s) \item col 33:
  river groundwater maximum monod loss (m3/s) } \item
  states.routing : model states related to routing (see
  awrar_runtimestep.c) \item parameters : model parameters
  (same as function input) \item config : model config
  (same as function input) \item inputs : model inputs
  (same as function input) }
}
\description{
  Run the river system component of the AWRA system
  (AWRA-R). Caution with the units !!! The following units
  are used \itemize{ \item Time is expressed in seconds
  \item Flows are expressed in m3/s \item Volumes are
  expressed in m3 \item Climate variables are expressed in
  mm/timestep } Caution with the Muskingum parameters !!!
  The following rules are used \itemize{ \item K = max(
  K,dt/2). If K=0, routing is not applied \item x is given
  by \link{get.actual.x} } The following model equations
  are used \itemize{ \item Routing : \eqn{dV/dt=I(t-\delta)
  - O} and \eqn{V=K (x I(t-\delta) + (1-x) O)} \item River
  loss (Monod function): \eqn{L=a\frac{Q}{b+Q}} \item River
  flow/area relationship : \eqn{A = \alpha Q^\beta} \item
  Floodplain volume/area relationship : \eqn{A = \alpha +
  \beta V} \item Anabranch loss : \eqn{L = a Q^b} \item
  Overbank flow : \eqn{Q_o = max(0,(Q-Q_o^*)^{\gamma})}
  \item Floodplain return flow : \eqn{Q_r = \frac{r}{dt} V}
  }
}
\examples{
# Model inputs
 inputs <- test.data1$inputs
 tribInflow<-inputs[,ncol(inputs)]
 inputs <- inputs[-ncol(inputs)]
 inputs$depth.to.gw<- 5 # add extra columns that are required
 inputs$river.depth<- -1 # timesteps with negative numbers will be calculated on the fly
 inputs$river.width<- -1 # timesteps with negative numbers will be calculated on the fly
 inputs<-cbind(inputs,tribInflow)
 config <- get.default.config()
 dt <- config[7]
 config[1] <- 1
 config[2] <- 1
 config[11] <- 0.6 # Second parameter of the Area/Volume relationship
 parameters <- unlist(test.data1$parameters)
 parameters[1] <- 1/50 # Return flow coefficient, i.e. time constant = dt*50
 parameters[5] <- 50   # Overbank flow threshold (m3/s)
 routingParams<-parameters[(length(parameters)-2):length(parameters)]
 parameters<-parameters[-((length(parameters)-2):length(parameters))]
 parameters<-c(parameters,0.2,0.000579,250,1,0.000011574) # adding extra parameters: aquifer specific yield, aquifer ksat, aquifer thickness, surface layer thickness, river conductance
 parameters<-c(parameters,routingParams)

 # Run model
 out <- awrar.run(parameters,config,inputs)

 # Plots
	layout(rbind(c(1,1),c(2,3)))
 matplot(cbind(inputs[,12],out$outflow),type="l",main="inflow/outflow",ylab="m3/s")
 plot(out$states.nonrouting[,3],type="l",main="flood volume",ylab="m3")
 matplot(out$states.nonrouting[,c(2,5)],type="l",main="flood overbank/return flow",ylab="m3/s")
}

