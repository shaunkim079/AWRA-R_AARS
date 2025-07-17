#' Run irrigation model
#'
#' Run the irrigation component of the AWRA system (AWRA-R).
#' Caution with the units !!! The following units are used
#'	\itemize{
#'		\item Time is expressed in seconds
#'		\item Flows are expressed in m3/s
#'		\item Volumes are expressed in m3 
#'		\item Climate variables are expressed in mm/timestep
#'	}
#' The following model equations are used
#'	\itemize{
#'		\item TODO
#'  }
#'
#'
#' @param parameters Model parameters ( vector \eqn{5 \times 1}) with
#'  \itemize{
#'		\item parameters[1] : monod alpha risk function (-)
#'		\item parameters[2] : monod beta risk function (-)
#'		\item parameters[3] : OFS threshold (m3/s)
#'		\item parameters[4] : max pumping capacity (m3/s)
#'		\item parameters[5] : pumping adjustment factor (m3/s)
#' }
#' @param config  Configuration data (vector \eqn{11 \times 1}) with
#'  \itemize{
#'		\item config[1] : time step duration (seconds)
#'		\item config[2] : max irrigation area (m2)
#'		\item config[3] : irrigation license volume (m3/s)
#'		\item config[4] : maximum On-Farm-Storage volume (m3/s)
#'		\item config[5] : maximum groundwater irrigation volume (m3/s)
#'		\item config[6] : irrigation efficiency (dimensionless)
#'		\item config[7] : returnflow coefficient (dimensionless)
#'		\item config[8] : soilCap (m)	
#'		\item config[9] : gamma (m2) 	
#'		\item config[10] : sigma (m) 
#'  	\item config[11] : ringtankAvgDepth (m) 
#' }
#' @param inputs  Irrigation input data (array \eqn{nday \times [7]}) with
#'	\itemize{
#'		\item col 1 : (irrig) rainfall (mm/time step)
#'		\item col 2 : (irrig) evap (mm/time step)
#'		\item col 3 : (irrig) irrigation allocation (-)
#'		\item col 4 : (irrig) Weighted Crop factor (-)
#'		\item col 5 : (irrig) Active irrigated area proportion (-)
#'		\item col 6 : (irrig) Julian day (-)
#'		\item col 7 : (irrig) reach inflow (m3/s)
#' }
#' @return A list object with the following structure:
#'	\itemize{
#'		\item diversion
#'		\item states
#'  	\itemize{
#'			\item col 1: licPro, (dimensionless)
#'			\item col 2: OFSpro, (dimensionless)
#'			\item col 3: GWpro, (dimensionless)
#'			\item col 4: areaActivePro_prec, (dimensionless)
#'			\item col 5: areaCurrent, (m2)
#'			\item col 6: demand, (m3/s)
#'			\item col 7: volOFS, (m3)
#'			\item col 8: volOFSin, (m3/s)
#'			\item col 9: areaOFS, (m2)
#'			\item col 10: soil, (m)
#'			\item col 11: runoff, (m3/s)
#'			\item col 12: gation, (m3/s)
#'			\item col 13: diversion, (m3/s)
#'			\item col 14: diversionCarryOver, (m3/s)
#'			\item col 15: drainage, (m3/s)
#'			\item col 16: volGWout, (m3/s)
#'			\item col 17: volOFSout, (m3/s)
#'			\item col 18: GWleft, (m3/s)
#'			\item col 19: licUsed, (m3/s)
#'			\item col 20: licWorking, (m3/s)
#'		}
#'	}
#' @export
#' @useDynLib awrar
#' @examples
#'  ##
irrigation.run <- function(parameters,configs,inputs,states=NULL){

  if(is.null(dim(inputs))) stop("inputs should have at least 2 dimensions")

	nday     <-as.integer(nrow(inputs))

	ninputs  <-as.integer(get.constant("NINPUTSIRRIG")) 

	nconfig  <-as.integer(get.constant("NCONFIGIRRIG"))

	npar     <-as.integer(get.constant("NPARIRRIG")) 

	nstates  <-as.integer(get.constant("NSTATESIRRIG"))

	if(nday==0) stop("nday = 0")
	if(ncol(inputs)!=ninputs) stop(sprintf("ncol(inputs)  (%d) !=ninputs (%d)\n", ncol(inputs),ninputs))

	if(length(configs) != nconfig) stop(sprintf("length(config)!=nconfig (%d)\n", nconfig))

	if(length(parameters)!=npar) stop(sprintf("length(parameters)!=npar (%d)\n", npar))

	ierr        <- as.integer(0)  
	nm.config 	<- names(configs)
  nm.parameters <- names(parameters)
	configs      <- as.double(unlist(configs)) 
	parameters  <- as.double(unlist(parameters)) 
	inputs      <- as.double(unlist(t(inputs)[1:(nday*ninputs)])) 

  # outputs
  if(is.null(states)){
    states<-as.double(rep(0,nday*nstates))
  } else {
    states<-as.double(states)
    if(length(states)!=nday*nstates){
      stop("The input states is not the right length (nday*nstates = ",nday*nstates,")")
    }
  }
  
	# Run the C code
  out <- .C("irrigation_run",
            ierr=ierr,
            nday=nday,nconfig=nconfig,ninputs=ninputs,
            npar=npar,nstates=nstates,
            config=configs,
            inputs_array=inputs,
            parameters=parameters,
            states_array=states)
  
  if(out$ierr>0) stop(sprintf("\nawrarirrig.run error - C code returned error %d\n",out$ierr))
  
  # reformat
  out$states <- t(matrix(out$states_array,nstates,nday))
  out$states_array <- NULL
  out$inputs <- t(matrix(out$inputs_array,ninputs,nday))
  out$inputs_array <- NULL
  
  out <- list(inputs = out$inputs, parameters = parameters, config = configs, states=out$states)
  
  #names(out$parameters)[1:5] <- c("alpha_irrig","beta_irrig","threshOFS","maxPump","pumpAdjust")
  names(out$parameters) <- nm.parameters
  
  names(out$config) <- nm.config
  
  colnames(out$inputs) <- paste("inp",1:ncol(out$inputs),sep=".")

  colnames(out$inputs)[1:8] <- c("rainfall","evap","allocation","WeightedKc","areaActivePro","JulianDay","reachInflow", "depthToGW")
  
  colnames(out$states)[1:23] <- c(
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
                                        
  class(out) <- "awrarirrig-run"        
                                        
  return(out)
}

