#' Return utility functions and data to deal with the parameters of the awrar model
#'
#' @param config Configuration object
#' @return a list object containing
#'	\itemize{
#'		\item inputnames : names of the \code{M} inputs
#'		\item parnames : names of the \code{N} parameters
#'		\item checkmodpar : function to check that a parameter sets has the correct length and names
#'		\item parlib : a matrix \code{P}x\code{N} of parameter containing an ensemble of \code{P} plausible parameter sets
#'		\item parbounds : a matrix \code{N}x2 containing the lower and upper boundary of the parameter
#'		\item true2trans : a function returning the transformed values of a parameter set (for optimisation) 
#'		\item trans2true : a function returning the real values of a transformed parameter set (for optimisation) 
#'	}
#' @export
#' @useDynLib awrar
#' @examples
#' pu <- awrarcalib.parutils()
#' 
#' 
#'
awrarcalib.parutils <- function(config=NULL){

	out <- list()

	# Has flood or not
	has.flood <- TRUE
	if(!is.null(config))
	  if(!is.null(config$has.flood))  has.flood <- config$has.flood

	# Has flood or not
	has.routing <- TRUE
	if(!is.null(config))
    if(!is.null(config$has.routing))  has.routing <- config$has.routing

	# RR model
	rrmod <- "AWRALSCALE"
	if(!is.null(config))
	  if(!is.null(config$rrmod))  rrmod <- config$rrmod

	# names and number of parameters
	out$modparnames <- c("floodReturn","monod1","monod2","flowThresh","flowExpon",
							"invv","diffCoeff","rr1","rr2","rr3","rr4")
	out$nmodpar <- length(out$modparnames)

	# inputs names
    out$inputnames <- "not set"

	# parameter check
	out$checkmodpar <- function(parameters)
	{
		if(!is.numeric(as.matrix(parameters)))
			stop("parameters are not numeric")
		if(length(parameters)!=out$nmodpar) 
			stop(paste("Wrong number of parameters : ",length(parameters),collapse=" "))
		if(prod( gsub("_trans$","",names(parameters))==out$modparnames )!=1) 
			stop(paste("Wrong parameter names : ",
				paste(names(parameters),collapse=" "),collapse=" "))		
	}


	# Transform parameter set for optimisation (transformation proposed by Charles Perrin)
	out$true2trans <- function(pin)
	{
		out$checkmodpar(pin)
		partrans <- log(pmax(0,pin)+1e-6)
		partrans[9] <- asinh(pin[9]) # Loss GR4J parameters

		names(partrans) <- paste(out$modparnames,"_trans",sep="")
		return(partrans)
	}

	# Get the true values of transformed parameter vector  (transformation proposed by Charles Perrin)
	out$trans2true <- function(pin)
	{
		partrue <- exp(pin)-1e-6
		partrue[9] <- sinh(pin[9]) # Loss GR4J parameters

		# Bounds
		if(has.flood)
		{
			partrue[1] <- max(0,min(1,partrue[1]))
			partrue[4] <- max(1e-1,min(2000,partrue[4]))
			partrue[5] <- max(0.1,min(0.9,partrue[5]))
		} else partrue[c(1,4,5)] <- c(0,10,0.5)
		
		partrue[2] <- max(0,min(1,partrue[2]))
		partrue[3] <- max(0,min(1e9,partrue[3]))
		partrue[6] <- max(0.1,min(5,partrue[6]))
		partrue[7] <- max(0,min(1,partrue[7]))

		if(rrmod=="AWRALSCALE")
		{
			partrue[8] <- max(0,min(20,partrue[8]))
			partrue[9:11] <- 0
		}
		if(rrmod=="GR4J")
		{
			partrue[8] <- max(0,min(10000,partrue[8]))
			partrue[9] <- max(-50,min(50,partrue[9]))
			partrue[10] <- max(0,min(1000,partrue[10]))
			partrue[11] <- max(0,min(5,partrue[11]))
		}
		names(partrue) <- out$modparnames
		return(partrue)
	}

	# Set parameter bounds
	if(is.null(config$modparbounds))
	{
		tmp <- rep(-100,out$nmodpar)
		names(tmp) <- paste(out$modparnames,"_trans",sep="")
		pb1 <- out$trans2true(tmp)		

		tmp <- rep(100,out$nmodpar)
		names(tmp) <- paste(out$modparnames,"_trans",sep="")
		pb2 <- out$trans2true(tmp)		

		out$modparbounds <- cbind(pb1,pb2)
	}
	else out$modparbounds <- config$modparbounds

	# Par constraints 
	if(is.null(config$modparconstraints)){
		out$modparconstraints <- function(parameters,data=NULL)
		{
			# -- does nothing by default ! --
			return(parameters)
		}
	} 
	else
	{
		out$modparconstraints <- config$modparconstraints
	}

	# Return a set of plausible parameters
	if(is.null(config$modparlib))
	{
		# Flood parameters
		floodReturn <- 0.3
		flowThresh <- 1e30
		flowExpon <- 0.5
		if(has.flood)
			flowThresh <- seq(0,500,length.out=3)

		# Monod
		monod1 <- seq(0,0.5,length.out=3)
		monod2 <- 1e6

		# Routing
		invv <- 0
		diffcoeff <- 0
		if(has.routing)
		{
			invv <- seq(0.5,2,length.out=3)
			diffCoeff <- seq(0,1,length.out=3)		
		}
		# RR
		rr1 <- seq(0.8,1.2,length.out=3)
		rr2 <- 0
		rr3 <- 0
		rr4 <- 0
		if(rrmod=="GR4J")
		{
			rr1 <- seq(100,500,length.out=3)
			rr2 <- seq(-2,0,length.out=3)
			rr3 <- seq(5,50,length.out=3)
			rr4 <- seq(0,3,length.out=3)
		}		
		out$modparlib <- expand.grid(floodReturn,monod1,monod2,flowThresh,
						flowExpon,invv,diffCoeff,rr1,rr2,rr3,rr4)
		colnames(out$modparlib) <- out$modparnames

	}
	else out$modparlib <- config$modparlib

	# Return a default parameter set
	default <- c(0.8,0,0,10,0.5,1,0.3,1,0,0,0)
	if(rrmod=="GR4J") default[8:11] <- c(370,-2,40,1)
	out$modpardefault <- default
	names(out$modpardefault) <- out$modparnames

	# Convert calibrated into C compatible parameters
	out$parcalib2parC <- function(parameters)
	{
		flood.ksat <- 0
		river.length <- 0
   	if(!is.null(config))
		{
       if(!is.null(config$flood.ksat))  flood.ksat <- config$flood.ksat
       if(!is.null(config$river.length)) river.length <- config$river.length
       if(!is.null(config$aquifer.specific.yield)) aquifer.specific.yield <- config$aquifer.specific.yield
       if(!is.null(config$aquifer.ksat)) aquifer.ksat <- config$aquifer.ksat
       if(!is.null(config$aquifer.thickness)) aquifer.thickness <- config$aquifer.thickness
       if(!is.null(config$surface.layer.thickness)) surface.layer.thickness <- config$surface.layer.thickness
       if(!is.null(config$river.conductivity)) river.conductivity <- config$river.conductivity
		}
		ninflow <- length(river.length)

		npar.nonrouting <- get.constant("NPARNONROUTING")
		npar.routing <- get.constant("NPARROUTING")
		npar     <-as.integer(ninflow*npar.routing+npar.nonrouting)

		# Reorganise parameters
		pars <- rep(0,npar)
		pars[1] <- parameters[1] # Flood return
		pars[2] <- flood.ksat # Flood Ksat
		pars[3] <- parameters[2]*parameters[3] # GW monod1
		pars[4] <- parameters[3] # GW monod2
		pars[5] <- parameters[4] # Overbank flow thresh
		pars[6] <- parameters[5] # overbank flow exponent
		pars[7] <- parameters[8] # runoff correction factor
    pars[8] <- aquifer.specific.yield
    pars[9] <- aquifer.ksat
    pars[10] <- aquifer.thickness
    pars[11] <- surface.layer.thickness
    pars[12] <- river.conductivity

		# routing pars
		invv <- parameters[6]
		alpha <- parameters[7]	
		LAG <- (1-alpha)*invv*river.length # Lag in sec
		K <- alpha*invv*river.length # Muskingum K
		X <- rep(0,ninflow) # Muskingum x
		pars[npar.nonrouting+(0:(ninflow-1))*npar.routing+1] <- LAG
		pars[npar.nonrouting+(0:(ninflow-1))*npar.routing+2] <- K
		pars[npar.nonrouting+(0:(ninflow-1))*npar.routing+3] <- X

		return(pars)
	}



	return(out)
}

