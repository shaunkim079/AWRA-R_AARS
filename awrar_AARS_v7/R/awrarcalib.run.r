#' Routine used to calibrate awrar
#'
#' The model is based on the C core of AWRA-R used in \link{awrar.run} with fixed Ksat.
#'
#' @param parameters Vector of model parameters (see \link{awrar.run})
#'		\itemize{
#'			\item p[1]: Flood return
#'			\item p[2]: GW monod 1
#'			\item p[3]: GW monod 2
#'			\item p[4]: Overbank flow thresh
#'			\item p[5]: Overbank flow exponent
#'			\item p[6]: Inverse velocity (s/m)
#'			\item p[7]: diffusion coefficient (-)
#'			\item p[8]: RR par 1 (e.g. scaling factor)
#'			\item p[9]: RR par 2 
#'			\item p[10]: RR par 3 
#'			\item p[11]: RR par 4 
#'		}
#' @param data Input data (see )
#'		\itemize{
#'			\item inputs : 	inputs to \link{awrar.run} function	
#'			\item P	: rainfall to be used by GR4J
#'			\item PE: APET to be used by GR4J
#'			\item config : config for \link{awrar.run} function	+
#'				\itemize{
#'					\item config[16] : Flood Ksat
#'					\item config[17] : River length(m)
#'				}
#'		}
#' @param istart Index of simulation start
#' @param iend Index of simulation end
#' @param stateini Initial filling level for the reservoirs of sacramento
#' @return A list object (nothing at the moment)
#' @export
#' @useDynLib awrar
awrarcalib.run <- function(parameters,data,istart=1,iend=nval,
						stateini=NULL){

  # Get the irrig inputs, config and parameters
  use_irrig<-as.numeric(data$use_irrig)
  irrigParameters<-data$irrigParameters
  irrigConfig<-data$irrigConfig
  irrigInputs<-data$irrigInputs

	# Get config
	if(is.null(data$config)) stop("is.null(data$config)")
	if(is.null(data$config$rrmod)) stop("is.null(data$config$rrmod)")
	rrmod <- data$config$rrmod
	if(!rrmod%in%c("AWRALSCALE","GR4J")) 
		stop(sprintf("rrmod %s not supported\n",rrmod))

	if(is.null(data$config$config.awrar)) stop("is.null(data$config$config.awrar)")
	config.awrar <- data$config$config.awrar
 
	if(is.null(data$config$flood.ksat)) stop("is.null(data$config$flood.ksat)")
    flood.ksat <- data$config$flood.ksat

	if(is.null(data$config$river.length)) stop("is.null(data$config$river.length)")
    river.length <- data$config$river.length

  if(is.null(data$is_headwater)) stop("is.null(data$is_headwater)")
    is_headwater<-data$is_headwater

	add.reservoir.carryover <- FALSE
    if(!is.null(data$config$add.reservoir.carryover)) 
		add.reservoir.carryover <- data$config$add.reservoir.carryover

	# Get input
	inputs <- data$inputs
	if(is.null(inputs)) stop("is.null(data$inputs)")
	nval <- nrow(inputs)

	# Restrict to simulation period
	if(iend>istart){
	  inputs <- inputs[istart:iend,]
	  irrigInputs<-irrigInputs[istart:iend,]
    
	} else {
	  stop("istart>=iend")
	} 

	# dimensions
	nconfig <- get.constant("NCONFIG")
	nstates.nonrouting <- get.constant("NSTATESNONROUTING")
	ninputs  <-ncol(inputs)
  	ninflow  <- ninputs - get.constant("NINPUTSNONROUTING")
	if(ninflow<0) stop("ninflow <0")
	if(length(river.length)!=ninflow) stop("length(river.length)!=ninflow")

	npar.nonrouting <- get.constant("NPARNONROUTING")
	npar.routing <- get.constant("NPARROUTING")
	npar     <-as.integer(ninflow*npar.routing+npar.nonrouting)
 
	if(length(config.awrar)!=nconfig) 
			stop(sprintf("length(onfig.awrar)=%d, should be %d\n",
				length(config.awrar),nconfig))

	# Reorganise parameters
	pars <- rep(0,npar)
	pars[1] <- parameters[1] # Flood return
	pars[2] <- flood.ksat # Flood Ksat
	pars[3] <- parameters[2]*parameters[3] # GW monod1
	pars[4] <- parameters[3] # GW monod2
	pars[5] <- parameters[4] # Overbank flow thresh
	pars[6] <- parameters[5] # overbank flow exponent

	pars[7] <- 1 # Default is no scaling (e.g. GR4J
	if(rrmod=="AWRALSCALE")	pars[7] <- parameters[8] # runoff correction factor
  
  # groundwater parameters
	pars[8]<-data$config$aquifer.specific.yield
	pars[9]<-data$config$aquifer.ksat
	pars[10]<-data$config$aquifer.thickness
	pars[11]<-data$config$surface.layer.thickness  
	pars[12]<-data$config$river.conductivity

	# routing pars
	invv <- parameters[6]
	alpha <- parameters[7]	
	LAG <- (1-alpha)*invv*river.length # Lag in sec
	K <- alpha*invv*river.length # Muskingum K
	X <- rep(0,ninflow) # Muskingum x
	pars[npar.nonrouting+(0:(ninflow-1))*npar.routing+1] <- LAG
	pars[npar.nonrouting+(0:(ninflow-1))*npar.routing+2] <- K
	pars[npar.nonrouting+(0:(ninflow-1))*npar.routing+3] <- X

	# Generate runoff with GR4J
	if(rrmod=="GR4J")
	{
		# Run GR4J
		p <- parameters[8:11]
		names(p) <- c("S","IGF","R","TB")
		out.rr <- gr4j.run(p,data,istart,iend)

		# Set inflow
		inputs[,8] <- out.rr$output
	}

	# Run awrar
	out <- awrar.run(pars,config.awrar,inputs,
                   use_irrig=use_irrig,irrigParameters=irrigParameters,
                   irrigConfig=irrigConfig,irrigInputs=irrigInputs,is_headwater=is_headwater)
# for(iii in 1:ncol(inputs)){
#   indy<-which(is.na(inputs[,iii]))
#   if(length(indy)>0){
#     stop()
#   }
# }
	# Get awrar output
	out$output <- out$outflow

	# Add reservoir carry over for reservoirs if required
	if(add.reservoir.carryover){
		out$output <- out$output+out$states.nonrouting[,19]
		out$outflow <- out$outflow+out$states.nonrouting[,19]
	}

	# Compute runoff
	out$runoff <- inputs[,8] * pars[7]

	# Save parameters
	out$parameters.original <- pars
	out$add.reservoir.carryover <- add.reservoir.carryover

	return(out)
}

