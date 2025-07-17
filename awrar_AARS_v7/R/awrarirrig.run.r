#' Run awrar model
#'
#' Run the river system component of the AWRA system (AWRA-R).
#' Caution with the units !!! The following units are used
#'	\itemize{
#'		\item Time is expressed in seconds
#'		\item Flows are expressed in m3/s
#'		\item Volumes are expressed in m3 
#'		\item Climate variables are expressed in mm/timestep
#'	}
#' Caution with the Muskingum parameters !!! The following rules are used
#'	\itemize{
#'		\item K = max( K,dt/2). If K=0, routing is not applied
#'		\item x is given by \link{get.actual.x}
#'	}
#' The following model equations are used
#'	\itemize{
#'		\item Routing : \eqn{dV/dt=I(t-\delta) - O} and \eqn{V=K (x I(t-\delta) + (1-x) O)}
#'		\item River loss (Monod function): \eqn{L=a\frac{Q}{b+Q}}
#'		\item River flow/area relationship : \eqn{A = \alpha Q^\beta}
#'		\item Floodplain volume/area relationship : \eqn{A = \alpha + \beta V}
#'		\item Anabranch loss : \eqn{L = a Q^b}
#'		\item Overbank flow : \eqn{Q_o = max(0,(Q-Q_o^*)^{\gamma})}
#'		\item Floodplain return flow : \eqn{Q_r = \frac{r}{dt} V}
#'  }
#'
#'
#' @param parameters Model parameters ( vector \eqn{[3 \times ninflow+4] \times 1}) with
#'	\itemize{
#'		\item parameters[1] : flood return flow coefficient (dimensionless)
#'		\item parameters[2] : flood Ksat (m/s)
#'		\item parameters[3] : Monod river loss parameter a (m3/s)
#'		\item parameters[4] : Monod river loss parameter b (m3/s)
#'		\item parameters[5] : overbank flow threshold, \eqn{Q_o^*} (m3/s)
#'		\item parameters[6] : exponent used to compute overbankflow thresold, \eqn{\gamma<1}
#'		\item parameters[7] : runoff correction factor (-)
#'  	\item parameters[8] : aquifer specific yield (-)
#'    \item parameters[9] : aquifer Ksat (m/s)
#'    \item parameters[10] : aquifer thickness (m)
#'    \item parameters[11] : surface layer thickness (m)
#'    \item parameters[12] : river conductance (m/s)
#'		\item parameters[13] : lag parameter \eqn{\delta} for inflow 1 (s)
#'		\item parameters[14] : Muskingum K for inflow 1  (s)
#'		\item parameters[15] : Muskingum x for inflow 1  (-)
#'		\item parameters[16] : lag parameter \eqn{\delta} for inflow 2 (s)
#'		\item parameters[17] : Muskingum K for inflow 2  (s)
#'		\item parameters[18] : Muskingum x for inflow 2  (-)
#'    \item ... same routing parameters (\eqn{\delta}, K and x) for other inflows
#' }
#' @param config  Configuration data (vector \eqn{14 \times 1}) with
#'	\itemize{
#'		\item config[1] : use routing (=1) or not (=0)
#'		\item config[2] : use flood model (=1) or not (=0)
#'		\item config[3] : use monod model (=1) or not (=0)
#'		\item config[4] : use reservoir model (=1) or not (=0)
#'		\item config[5] : use ungauged inflows (=1) or not (=0)
#'		\item config[6] : use anabranch (=1) or not (=0)
#'		\item config[7] : time step duration (seconds)
#'		\item config[8] : river area/volume relationship, \eqn{\alpha}
#'		\item config[9] : river area/volume relationship, \eqn{\beta}
#'		\item config[10] : floodplain area/volume relationship, \eqn{\alpha} (not used at the moment)
#'		\item config[11] : floodplain area/volume relationship, \eqn{\beta}
#'		\item config[12] : anabranch loss - upstream of floodplain, parameter a
#'		\item config[13] : anabranch loss - upstream of floodplain, parameter b
#'		\item config[14] : anabranch loss - downstream of floodplain, parameter a
#'		\item config[15] : anabranch loss - downstream of floodplain, parameter b
#'  	\item config[16] : floodplain length (m)
#'    \item config[17] : river depth/flow relationship, \eqn{\alpha}
#'    \item config[18] : river depth/flow relationship, \eqn{\beta}
#' }
#' @param inputs  Input data (array \eqn{nval \times [11+ninflow]}) with
#'	\itemize{
#'		\item col 1 : rainfall river  (mm/time step)
#'		\item col 2 : evap river       (mm/time step)
#'		\item col 3 : rainfall floodplain  (mm/time step)
#'		\item col 4 : evap floodplain     (mm/time step)
#'		\item col 5 : irrigation diversion  (m3/s)
#'		\item col 6 : irrigation return flow  (m3/s)
#'		\item col 7 : diversion for urban water supply  (m3/s)
#'		\item col 8 : ungauged inflow - top   (m3/s)
#'		\item col 9 : ungauged inflow - bottom  (m3/s)
#'		\item col 10 : reservoir volume (m3)
#'		\item col 11 : reservoir area   (m2)
#'  	\item col 12 : depth to groundwater    (m)
#'    \item col 13 : river depth    (m)
#'    \item col 14 : river width    (m)
#'		\item col 15 : inflow 1          (m3/s)
#'		\item col 16 : inflow 2          (m3/s)
#'    \item col 17 : ... (other inflows) (m3/s)
#'    
#' }
#' @return A list object with the following structure:
#'	\itemize{
#'		\item outflow : Flow at the downstream end of the reach
#'		\item states.nonrouting : model states not related to routing  (see awrar_runtimestep.c):
#'		\itemize{
#'			\item col 1: outflow from reach   (m3/s)
#'			\item col 2: overbank_flow from river to floodplain  (m3/s)
#'			\item col 3: floodplain volume (m3),
#'			\item col 4: floodplain area (m2), 
#'			\item col 5: return flow from floodplain to river  (m3/s)
#'			\item col 6: river rainfall flux  (m3/s)
#'			\item col 7: river evap flux     (m3/s)
#'			\item col 8: floodplain rainfall flux (m3/s)
#'			\item col 9: floodplain evap flux   (m3/s)
#'			\item col 10: floodplain groundwater loss (m3/s)
#'			\item col 11: river groudwater loss (monod)   (m3/s)
#'			\item col 12: Top anabranch loss (upstream of floodplain, m3/s)
#'			\item col 13: Bottom anabranch loss (downstream of floodplain, m3/s)
#'			\item col 14: previous reservoir volume (m3)
#'			\item col 15: previous reservoir area   (m2)
#'			\item col 16: rainfall flux on reservoir (m3/s)
#'			\item col 17: evap flux on reservoir (m3/s)
#'			\item col 18: reservoir contribution (m3/s)
#'			\item col 19: carryover reservoir flux (m3/s) 
#'			\item col 20: outflow 2 (m3/s) 
#'			\item col 21: outflow 3 (m3/s) 
#'			\item col 22: outflow 4 (m3/s) 
#'			\item col 23: outflow 5 (m3/s) 
#'			\item col 24: outflow 6 (m3/s) 
#'			\item col 25: outflow 7 (m3/s) 
#'			\item col 26: river volume (m3)
#'  		\item col 27: floodplain groundwater max change in storage (m3/s)
#'			\item col 28: floodplain groundwater outflow (m3/s)
#'			\item col 29: floodplain groundwater maximum infiltration (m3/s)
#'			\item col 30: river groundwater max change in storage (m3/s)
#'			\item col 31: river groundwater outflow (m3/s)
#'			\item col 32: river groundwater maximum infiltration (m3/s)
#'			\item col 33: river groundwater maximum monod loss (m3/s)
#'		}
#'		\item states.routing : model states related to routing (see awrar_runtimestep.c)
#'		\item parameters : model parameters (same as function input)
#'		\item config : model config (same as function input)
#'		\item inputs : model inputs (same as function input)
#'  }
#' @export
#' @useDynLib awrar
#' @examples
#'  # Model inputs
#'  inputs <- test.data1$inputs
#'  tribInflow<-inputs[,ncol(inputs)]
#'  inputs <- inputs[-ncol(inputs)]
#'  inputs$depth.to.gw<- 5 # add extra columns that are required
#'  inputs$river.depth<- -1 # timesteps with negative numbers will be calculated on the fly
#'  inputs$river.width<- -1 # timesteps with negative numbers will be calculated on the fly
#'  inputs<-cbind(inputs,tribInflow)
#'  config <- get.default.config()
#'  dt <- config[7]
#'  config[1] <- 1
#'  config[2] <- 1
#'  config[11] <- 0.6 # Second parameter of the Area/Volume relationship
#'  parameters <- unlist(test.data1$parameters)
#'  parameters[1] <- 1/50 # Return flow coefficient, i.e. time constant = dt*50
#'  parameters[5] <- 50   # Overbank flow threshold (m3/s)
#'  routingParams<-parameters[(length(parameters)-2):length(parameters)]
#'  parameters<-parameters[-((length(parameters)-2):length(parameters))]
#'  parameters<-c(parameters,0.2,0.000579,250,1,0.000011574) # adding extra parameters: aquifer specific yield, aquifer ksat, aquifer thickness, surface layer thickness, river conductance
#'  parameters<-c(parameters,routingParams)
#'
#'  # Run model
#'  out <- awrar.run(parameters,config,inputs)
#'
#'  # Plots
#'	layout(rbind(c(1,1),c(2,3)))
#'  matplot(cbind(inputs[,12],out$outflow),type="l",main="inflow/outflow",ylab="m3/s")
#'  plot(out$states.nonrouting[,3],type="l",main="flood volume",ylab="m3")
#'  matplot(out$states.nonrouting[,c(2,5)],type="l",main="flood overbank/return flow",ylab="m3/s")
#'
awrarirrig.run <- function(parameters,config,inputs,states=NULL,use_irrig,irrigParameters=NULL,irrigConfig=NULL,irrigInputs=NULL,irrigStates=NULL){
  
  if(is.null(dim(inputs))) stop("inputs should have at least 2 dimensions")
  if(length(which(config[1:6]!=0 & config[1:6]!=1))>0)  
        stop("First 6 values of config should be 0 or 1 (they are flags to activate awra-r components)")


	# inputs
	nval     <-as.integer(nrow(inputs))

	ninputs  <-as.integer(ncol(inputs))
  ninflow  <- ninputs - get.constant("NINPUTSNONROUTING")
	if(ninflow<0) stop("ninflow <0")

	nconfig  <-as.integer(get.constant("NCONFIG")) 

	npar     <-as.integer(ninflow*get.constant("NPARROUTING")+
							get.constant("NPARNONROUTING")) 

	nstates.nonrouting  <- get.constant("NSTATESNONROUTING")
	nstates.routing  	<- get.constant("NSTATESROUTING")
	nstates  <-as.integer(ninflow*nstates.routing+nstates.nonrouting) 

	if(nval==0) stop("nval = 0")
	if(length(config)!=nconfig) stop(sprintf("length(config)!=nconfig (%d)\n",
                                          nconfig))

	if(length(parameters)!=npar) stop(sprintf("length(parameters)!=npar (%d)\n",
                                          npar))

  ierr        <- as.integer(0)  
  nm.config 	<- names(config)
  config      <- as.double(unlist(config)) 
  parameters  <- as.double(unlist(parameters)) 
  inputs      <- as.double(unlist(t(inputs)[1:(nval*ninputs)])) 
  
  # outputs
  if(is.null(states)){
    states<-as.double(rep(0,nval*nstates))
  } else {
    states<-as.double(states)
    if(length(states)!=nval*nstates){
      stop("The input states is not the right length (nval*nstates = ",nval*nstates,")")
    }
  }

  #check irrigation model
	if(use_irrig == 1){
	  if(is.null(dim(irrigInputs))) stop("irrigation inputs should have at least 2 dimensions")
	  
    nirrigval <- as.integer(nrow(irrigInputs))
  	if(nirrigval==0) stop("nirrigval = 0")
  	if(nval != nirrigval) stop(sprintf("number of days in irrigation (%d) != that in inputs (%d)\n", nirrigval, nval))
  
  	nirriginputs  <-as.integer(ncol(irrigInputs))
  	
  	nirrigconfig  <-as.integer(get.constant("NCONFIGIRRIG")) 
  	if(length(irrigConfig)!=nirrigconfig) stop(sprintf("length(irrigConfig)!=nirrigconfig (%d)\n", nirrigconfig))
  	
  	nirrigpar     <-as.integer(get.constant("NPARIRRIG"))
  	if(length(irrigParameters)!=nirrigpar) stop(sprintf("length(irrigParameters)!=nirrigpar (%d)\n", nirrigpar))
  
  	nirrigstates  <- get.constant("NSTATESIRRIG")

    nm.irrigConfig 	<- names(irrigConfig)
    irrigConfig      <- as.double(unlist(irrigConfig)) 
    irrigParameters  <- as.double(unlist(irrigParameters)) 
    irrigInputs      <- as.double(unlist(t(irrigInputs)[1:(nirrigval*nirriginputs)])) 

    if(is.null(irrigStates)){
      irrigStates<-as.double(rep(0,nirrigval*nirrigstates))
    } else {
      irrigStates<-as.double(irrigStates)
      if(length(irrigStates)!=nirrigval*nirrigstates){
        stop("The input irrigation states is not the right length (nirrigval*nirrigstates = ",nirrigval*nirrigstates,")")
      }
    }
	}else{
	  irrigConfig      <- vector() 
	  irrigParameters  <- vector()
	  irrigInputs      <- vector()
    irrigStates   <-    vector()
	}
  
	# Run the C code
	#void awrarirrig_run(
	#		int * ierr,
	#		int *nval, int *nconfig, int * ninputs,int * npar,
	#	    int * nstates,
	#		double * config,
	#		double * inputs_array,
	#		double * parameters,
	#		double * states_array,
	#		double * use_irrig,
	#		double * irrigConfig,
	#		double * irrigInputs_array,
	#		double * irrigParameters,
	#		double * irrigStates_array)	
	out <- .C("awrarirrig_run",
    ierr=ierr,
    nval=nval,nconfig=nconfig,ninputs=ninputs,
	  npar=npar,nstates=nstates,
    config=config,
    inputs_array=inputs,
    parameters=parameters,
    states_array=states,
	  use_irrig = use_irrig,
	  irrigConfig = irrigConfig,
	  irrigInputs_array = irrigInputs,
	  irrigParameters = irrigParameters,
	  irrigStates_array = irrigStates	
	)

	if(out$ierr>0) stop(sprintf("\nawrar.run error - C code returned error %d\n",out$ierr))
	
	# reformat
	out$states <- t(matrix(out$states_array,nstates,nval))
	states_array<-out$states_array
	out$states_array <- NULL
	out$inputs <- t(matrix(out$inputs_array,ninputs,nval))
	out$inputs_array <- NULL
	
  if(use_irrig == 1){
    out$irrigStates <- t(matrix(out$irrigStates_array,nirrigstates,nirrigval))
    out$irrigStates_array <- NULL
    out$irrigInputs <- t(matrix(out$irrigInputs_array,nirriginputs,nirrigval))
    out$irrigInputs_array <- NULL
  }
  
  out <- list(outflow=out$states[,1],
              inputs = out$inputs,
              parameters = parameters,
              config = config,
              states.nonrouting=out$states[,1:nstates.nonrouting],
              states.routing=out$states[,(nstates.nonrouting+1):nstates],
              states_array=states_array,
              irrigInputs = out$irrigInputs,
              irrigParameters = irrigParameters,
              irrigConfig = irrigConfig,
              irrigStates=out$irrigStates)	
  
  names(out$parameters)[1:6] <- c("flood.returnflow.coefficient",
                                  "flood.ksat","river.monod1","river.monod2","overbank.flow.threshold",
                                  "flood.gamma")
  names(out$config) <- nm.config

  colnames(out$inputs) <- paste("inp",1:ncol(out$inputs),sep=".")
  colnames(out$inputs)[1:11] <- c("river.rainfall",
                                  "river.evap","floodplain.rainfall","floodplain.evap",
                                  "irrigation.diversion","irrigation.returnflow",
                                  "urban.diversion","ungauged.inflow.top","ungauged.inflow.bottom",
                                  "reservoir.volume","reservoir.area")
  
  names.states.nonrouting<-c("outflow","overbank.flow",
                             "floodplain.volume","floodplain.area","floodplain.returnflow",
                             "river.rainfall.flux","river.evap.flux",
                             "floodplain.rainfall.flux","floodplain.evap.flux",
                             "floodplain.groundwater.loss","river.groundwater.loss",
                             "top.anabranch.loss","bottom.anabranch.loss",
                             "previous.reservoir.vol","previous.reservoir.area",
                             "reservoir.rainfall.flux","reservoir.evap.flux",
                             "reservoir.contribution","reservoir.carryover.flux",
                             sprintf("outflow%d",2:7),"river.volume",
                             "floodplain.groundwater.max.change.storage",
                             "floodplain.groundwater.outflow",
                             "floodplain.groundwater.max.infiltration",
                             "river.groundwater.max.change.storage",
                             "river.groundwater.outflow",
                             "river.groundwater.max.infiltration",
                             "river.groundwater.max.monod.loss")
  if(is.null(dim(out$states.nonrouting))){
    names(out$states.nonrouting)[1:33] <- names.states.nonrouting
  } else {
    colnames(out$states.nonrouting)[1:33] <- names.states.nonrouting
  }

  if(use_irrig == 1){
  
    names(out$irrigParameters)[1:5] <- c("alpha_irrig","beta_irrig","threshOFS","maxPump","pumpAdjust")
  
  	names(out$irrigConfig) <- nm.irrigConfig
  
  
  	colnames(out$irrigInputs) <- paste("inp",1:ncol(out$irrigInputs),sep=".")
  	colnames(out$irrigInputs)[1:8] <- c("rainfall", "evap", "irrigation.allocation","Weighted.Crop.factor","areaActivePro","JulianDay", "reach.inflow", "depthToGW")
  
  	colnames(out$irrigStates)[1:23] <- c(
  		"licPro",
  		"OFSpro",
  		"GWpro",
  		"areaActivePro_prec",
  		"areaCurrent",
  		"demand",		
  		"volOFS",			
  		"volOFSin",			
  		"areaOFS",			
  		"soil",				
  		"runoff",			
  		"gation",			
  		"diversion",				
  		"diversionCarryOver",
  		"drainage",			
  		"volGWout",			
  		"volOFSout",				
  		"GWleft",			
  		"licUsed",			
  		"licWorking",		
  		"rechargeGWir",
  		"deltaS",
      "Infiltration")
  }
  
  class(out) <- "awrar-run"

	return(out)
}

