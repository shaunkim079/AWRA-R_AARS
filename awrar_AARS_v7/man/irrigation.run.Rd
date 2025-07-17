\name{irrigation.run}
\alias{irrigation.run}
\title{Run irrigation model}
\usage{
  irrigation.run(parameters, configs, inputs,
    states = NULL)
}
\arguments{
  \item{parameters}{Model parameters ( vector \eqn{5 \times
  1}) with \itemize{ \item parameters[1] : monod alpha risk
  function (-) \item parameters[2] : monod beta risk
  function (-) \item parameters[3] : OFS threshold (m3/s)
  \item parameters[4] : max pumping capacity (m3/s) \item
  parameters[5] : pumping adjustment factor (m3/s) }}

  \item{config}{Configuration data (vector \eqn{11 \times
  1}) with \itemize{ \item config[1] : time step duration
  (seconds) \item config[2] : max irrigation area (m2)
  \item config[3] : irrigation license volume (m3/s) \item
  config[4] : maximum On-Farm-Storage volume (m3/s) \item
  config[5] : maximum groundwater irrigation volume (m3/s)
  \item config[6] : irrigation efficiency (dimensionless)
  \item config[7] : returnflow coefficient (dimensionless)
  \item config[8] : soilCap (m) \item config[9] : gamma
  (m2) \item config[10] : sigma (m) \item config[11] :
  ringtankAvgDepth (m) }}

  \item{inputs}{Irrigation input data (array \eqn{nday
  \times [7]}) with \itemize{ \item col 1 : (irrig)
  rainfall (mm/time step) \item col 2 : (irrig) evap
  (mm/time step) \item col 3 : (irrig) irrigation
  allocation (-) \item col 4 : (irrig) Weighted Crop factor
  (-) \item col 5 : (irrig) Active irrigated area
  proportion (-) \item col 6 : (irrig) Julian day (-) \item
  col 7 : (irrig) reach inflow (m3/s) }}
}
\value{
  A list object with the following structure: \itemize{
  \item diversion \item states \itemize{ \item col 1:
  licPro, (dimensionless) \item col 2: OFSpro,
  (dimensionless) \item col 3: GWpro, (dimensionless) \item
  col 4: areaActivePro_prec, (dimensionless) \item col 5:
  areaCurrent, (m2) \item col 6: demand, (m3/s) \item col
  7: volOFS, (m3) \item col 8: volOFSin, (m3/s) \item col
  9: areaOFS, (m2) \item col 10: soil, (m) \item col 11:
  runoff, (m3/s) \item col 12: gation, (m3/s) \item col 13:
  diversion, (m3/s) \item col 14: diversionCarryOver,
  (m3/s) \item col 15: drainage, (m3/s) \item col 16:
  volGWout, (m3/s) \item col 17: volOFSout, (m3/s) \item
  col 18: GWleft, (m3/s) \item col 19: licUsed, (m3/s)
  \item col 20: licWorking, (m3/s) } }
}
\description{
  Run the irrigation component of the AWRA system (AWRA-R).
  Caution with the units !!! The following units are used
  \itemize{ \item Time is expressed in seconds \item Flows
  are expressed in m3/s \item Volumes are expressed in m3
  \item Climate variables are expressed in mm/timestep }
  The following model equations are used \itemize{ \item
  TODO }
}
\examples{
##
}

