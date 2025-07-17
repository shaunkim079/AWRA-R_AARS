#' Get the water accounts from simulations produced by \link{awrar.run} and \link{irrigation.run}
#'
#' The accounts are produced based on the template \link{accounts.template}.
#'
#' @param river.run A run obtained from \link{awrar.run}
#' @param irrig.run A run obtained from \link{irrigation.run}
#' @param start Start date of the simulation with format MM-DD
#' @param start.water.year Start of the water year
#' @return a data frame containing the accounts
#' @export
#' @useDynLib awrar
#' @examples
#' # Awrar run
#' inputs <- test.data1$inputs
#' config <- get.default.config()
#' dt <- config[7]
#' config[1] <- 1
#' config[2] <- 1
#' config[11] <- 0.6 # Second parameter of the Area/Volume relationship
#' parameters <- unlist(test.data1$parameters)
#' parameters[1] <- 1/50 				# Return flow coefficient
#' parameters[5] <- 50 					# Overbank flow threshold
#' river.run <- awrar.run(parameters,config,inputs)
#' 
#' # dummy irrigation data set
#' nval <- length(river.run$outflow)
#' zero <- rep(0,nval)
#' data <- data.frame(ofs=zero,
#'			swdiversion=zero,
#'			gwdiversion=zero,
#'			floodharvest=zero,
#'			returnflow=zero,
#'			evapofs=zero,
#'			rainfallofs=zero,
#'			evapcrop=zero,
#'			rainfallcrop=zero,
#'			applicationcrop=zero,
#'			gwlossofs=zero)
#'	irrig.run <- data2irrigation.run(data)
#'
#'  accounts <- get.accounts(river.run,irrig.run,start="1970-01-01")
#'
get.accounts <- function(river.run,irrig.run,start,start.water.year="07-01"){

	# Check class
	if(!inherits(river.run,"awrar-run")) stop("class of river.run object is not awrar-run")
	if(!inherits(irrig.run,"irrigation-run")) 
		stop("class of irrig.run object is not irrigation-run. Use data2irrigation.run to convert data")
	if(!inherits(start,"Date"))
	{
		err <- try(start <- as.Date(start),silent=TRUE)
		if(inherits(err,"try-error")) stop("Failed converting start to a date")
	}

	# Simulation duration
	outflow <- river.run$outflow
	nval <- length(outflow)
	div <- irrig.run$diversion
	if(nval!=length(div)) stop("river.run and irrig.run do not have the same length")

	day <- seq(start,by="day",length.out=nval)
	#year <- as.Date(format(day,"%Y-

	# Water years
	water.year <- cumsum(as.integer(format(day,
			"%m-%d")==start.water.year))-1+as.integer(format(day[1],"%Y"))
	water.year <- as.Date(paste(water.year,start.water.year,sep="-"))

	# get model states
	tws <- river.run$inputs[,7]
	river.runoff <- rowSums(river.run$inputs[,8:9])
	sum.inflow <- river.run$inputs[,12:ncol(river.run$inputs)]
	if(!is.null(dim(sum.inflow))) 
    sum.inflow <- rowSums(apply(river.run$inputs[,12:ncol(river.run$inputs)],2,function(v) pmax(v,0)))
	states.river <- river.run$states.nonrouting
	states.irrig <- irrig.run$states

	nstates.routing <- get.constant("NSTATESROUTING")
	river.volume <- river.run$states.nonrouting[,26] 

	# loop through years
	acc <- accounts.template[,1:5]
	water.year.unique <- unique(water.year)
	for(i in 1:length(water.year.unique))
	{
		# get index
		ii <- which(water.year==water.year.unique[i])
		i1 <- min(ii)
		i2 <- max(ii)

		# Fill up accounts
		acc <- data.frame(acc,rep(NA,nrow(acc)))
		acc[acc$name=="river.store",5+i] <- (river.volume[i2]-river.volume[i1])*86.4*(i2-i1+1)
		acc[acc$name=="river.rainfall",5+i] <- sum(states.river[ii,6]*86.4) # m3/s -> ML/d
		acc[acc$name=="river.runoff",5+i] <- sum(river.runoff[ii]*86.4)
		acc[acc$name=="river.inflow",5+i] <- sum(sum.inflow[ii]*86.4) # m3/s -> ML/d
		acc[acc$name=="river.irrigationreturnflow",5+i] <- NA
		acc[acc$name=="river.evap",5+i] <- sum(states.river[ii,7]*86.4) # m3/s -> ML/d
		acc[acc$name=="river.outflow",5+i] <- sum(states.river[ii,1]*86.4) # m3/s -> ML/d
		acc[acc$name=="river.transmissionloss",5+i] <- sum(states.river[ii,11]*86.4) # m3/s -> ML/d
		acc[acc$name=="river.tws",5+i] <- sum(tws[ii])
		acc[acc$name=="river.floodoverbankflow",5+i] <- sum(states.river[ii,2]*86.4) # m3/s -> ML/d
		acc[acc$name=="river.floodreturnflow",5+i] <- sum(states.river[ii,5]*86.4) # m3/s -> ML/d
		acc[acc$name=="river.swdiversion",5+i] <- NA
		acc[acc$name=="floodplain.store",5+i] <- (states.river[i2,3]-states.river[i1,3])*86.4*(i2-i1+1)
		acc[acc$name=="floodplain.rainfall",5+i] <- sum(states.river[ii,8]*86.4) # m3/s -> ML/d
		acc[acc$name=="floodplain.overbankflow",5+i] <- acc[acc$name=="river.floodoverbankflow",5+i]
		acc[acc$name=="floodplain.evap",5+i] <- sum(states.river[ii,9]*86.4) # m3/s -> ML/d
		acc[acc$name=="floodplain.returnflow",5+i] <- acc[acc$name=="river.floodreturnflow",5+i]
		acc[acc$name=="floodplain.gwloss",5+i] <- sum(states.river[ii,10]*86.4) # m3/s -> ML/d
		acc[acc$name=="floodplain.floodharvest",5+i] <- NA
		acc[acc$name=="reservoir.store",5+i] <- NA
		acc[acc$name=="reservoir.inflow",5+i] <- NA
		acc[acc$name=="reservoir.rainfall",5+i] <- NA
		acc[acc$name=="reservoir.outflow",5+i] <- NA
		acc[acc$name=="reservoir.evap",5+i] <- NA
		acc[acc$name=="reservoir.gwloss",5+i] <- NA
		acc[acc$name=="irrigation.ofs",5+i] <- NA
		acc[acc$name=="irrigation.swdiversion",5+i] <- NA
		acc[acc$name=="irrigation.gwdiversion",5+i] <- NA
		acc[acc$name=="irrigation.floodharvest",5+i] <- NA
		acc[acc$name=="irrigation.returnflow",5+i] <- NA
		acc[acc$name=="irrigation.evapofs",5+i] <- NA
		acc[acc$name=="irrigation.rainfallofs",5+i] <- NA
		acc[acc$name=="irrigation.evapcrop",5+i] <- NA
		acc[acc$name=="irrigation.rainfallcrop",5+i] <- NA
		acc[acc$name=="irrigation.applicationcrop",5+i] <- NA
		acc[acc$name=="irrigation.gwlossofs",5+i] <- NA
	}
	colnames(acc)[6:ncol(acc)] <- sprintf("water.year.%s",water.year.unique) 
	class(acc) <- "awrar-accounts"
	return(acc);
}

