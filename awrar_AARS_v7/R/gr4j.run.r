#' Run the GR4J model
#'
#'
#' @param parameters Vector of model parameters. The vector components should be named 
#'  \itemize{
#'    \item names(parameters)[1] : "S" (Production store)
#'    \item names(parameters)[2] : "IGF" (Groundwater exchange)
#'    \item names(parameters)[3] : "R" (Routing store)
#'    \item names(parameters)[4] : "TB" (Time base of unit hydrograph)
#'  }
#' @param data a list object with the following structure 
#'		\itemize{
#'				\item P : Timeseries of rainfall (mm/time step, e.g mm/day for daily rainfall)
#'				\item PE : Timeseries of potential evapotranspiration (mm/time step)
#'  			\item config : A list object containing the following elements
#'  	    \itemize{
#'          \item dt : Time step length in seconds (default is 86400 sec)
#'          \item area : Catchment area in km2 (default is dt/1e3 so that the model produces a value in mm/timestep)
#'        }
#'    		
#'		}
#' @param istart Index of simulation start. Default is 1
#' @param iend Index of simulation end. Default is the number of time steps in data
#' @param stateini Initial filling level for the reservoirs
#' @return A list object having the following structure
#'    \itemize{
#'      \item output : Vector of calculated streamflow (m3/s). Warning ! This is NOT runoff.
#'      \item states : Matrix of model states
#'    }
#' @export
#' @useDynLib awrar
#' @examples
#' # Get data from first example
#' data <- rr.data01
#' d <- list(P=data.frame(data$data[,2]),PE=data.frame(data$data[,3]),config=list(idmod=1))
#'
#' # get parameters from first example
#' p <- data$params.gr4j
#' names(p) <- c("S","IGF","R","TB")
#' 
#' # Run model
#' out <- gr4j.run(p,d)
#' 
#' # plot results
#' plot(out$output,type="l",ylab="Simulated flow (m3/s)")
#' 
gr4j.run <- function(parameters,data,istart=1,iend=nval,
						stateini=NULL){

	dt <- 86400
	if(!is.null(data$config$dt)) dt <- data$config$dt
	area <- dt*1e-3
	if(!is.null(data$config$area)) area <- data$config$area
	mPmE <- c(5,3)
	if(!is.null(data$config$mPmE)) mPmE <- data$config$mPmE

	if(is.null(data$P)) stop("data$P is null")
	if(length(grep("data.frame",class(data$P)))==0) 
			stop("data$P is not a data.frame")
  
  	Qref <- 1 # Not used here
  	idmod <- 1 # Not used here 
  	version.model <- 0 # Not used here 
	version.parbound <- 0 # Not used here 
    
	# Dimensions
	nval <- nrow(data$P)
	nstates <- 12
	ninputs <- 2

	if(is.null(data$PE)) stop("data$PE is null")
	if(length(grep("data.frame",class(data$PE)))==0) 
			stop("data$PE is not a data.frame")
	if(nrow(data$PE)!=nval) stop("nrow(PE) != nrow(P)")

	# number and names of parameters
	ierr <- as.integer(0)

	config <- as.double(rep(0,get.constant("NCONFIGMAX")))
	config[1] <- as.double(dt)  #: time step length (seconds)
	config[2] <- as.double(area)  #: (for rr models) catchment area (km2)
	config[3] <- as.double(mPmE[1])  #: (for GR4J) mean annual P-PE where P-PE>=0 (mm)
	config[4] <- as.double(mPmE[2])  #: (for GR4J) mean annual PE-P where PE-P>=0 (mm)
	config[5] <- as.double(Qref)  #: mean annual outflow (m3/s)
	config[17] <- as.double(nstates) # nstates
	config[18] <- as.double(ninputs) #	ninputs
	config[7] <- as.double(version.model) #: (for sacramento) UH type (0=Pure lag, 1=2UH, 2=triangle)
	config[15] <- as.double(idmod) 

	config[26] <- as.double(version.parbound) # parbound version

	nm <- .C("gr4j_namepar",ierr=as.integer(0),npar=as.integer(4),
            config=config,names=as.character(rep("X",50)))
	names <- nm$names[nm$names!="X"]
	npar <- length(names)
	config[16] <- as.double(npar) # napr
	
	if(length(parameters)!=npar) stop("length(parameters)!=npar")
	sd <- setdiff(names(parameters),names)
	if(length(sd)!=0) 
		stop("wrong parameter names\n",sprintf("\t%s is not valid\n",sd))
	
	# inputs - dimensions
	nsim <-as.integer(iend-istart+1)
	isim <- istart:iend

	ierr <- as.integer(0)
	store_states <- as.integer(1)
	overwrite_output <- as.integer(0)

	partrue <- as.double(parameters)
	inputs <- as.double(t(cbind(data$P[isim,],data$PE[isim,]))[1:(2*nsim)])
	output <- as.double(rep(0,nsim))
	states <- as.double(rep(0,nsim*nstates))

	# C code --
	#void run_comp(int * ierr, int * nval, 
	#	int * store_states, int *overwrite_output,	
	#	double * config,	
	#	double * partrue,
	#	double * inputs,
	#	double * states,
	#	double * output)
	out <- .C("gr4j_runtimeseries",
    ierr=ierr,nval=nsim,
		store_states=store_states,
		overwrite_output,	
		config=config,	
		partrue=partrue,
		inputs=inputs,
		states=states,
		output=output)

	if(out$ierr>0)
	{
		mess <- sprintf("error - c codes return ierr=%d",out$ierr)
		cat("nError rr.run :",mess,"\n")
	}

	# reformat
	out$states <- t(matrix(out$states,nstates,nsim))	
  colnames(out$states) <- c("streamflow","S.store","R.store",
                            "runoff","exchange","actual.et","net.rainfall",
                            "direct.runoff","routed.runoff","percolation",
                            "input.direct.branch","input.routing.store")
	out$dt <- dt
	out$area <- area
	out$mPmE <- mPmE
	out$version.model <- version.model
	out$version.parbound <- version.parbound

	return(out)
}

