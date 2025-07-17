#' Compute performace statistics from an awra-r simulation (see \link{awrar.run})
#'
#' The statistics are
#' \itemize{
#'	\item bias: \eqn{B = \frac{|\sum_i q_i - \sum_i \hat{q}_i|}{\sum_i q_i}}
#'	\item max.yearly.bias: \eqn{BM = \max_k\left(\frac{|\sum_{i\in y_k} q_i - \sum_{i\in y_k}  \hat{q}_i|}{\sum_{i\in y_k}  q_i}\right)} with k the year number.
#'	\item nse: \eqn{NSE = 1- \frac{\sum_i (q_i- \hat{q}_i)^2}{\sum_i (q_i-\bar{q})^2}}
#'	\item nse.monthly: NSE on monthly flows
#'	\item nse.log: daily NSE on log transformed flow where \eqn{logq = \log(q+10^{-4})} with q in m3/s.
#' }
#'
#' @param obs Observed daily streamflow time series. Data frame with 2 columns named day (date) and flow (flow in cumecs)
#' @param run A run obtained from \link{awrar.run}
#' @param start Start date of the simulation
#' @param start.water.year Start of the water year with format MM-DD
#' @return a data frame containing the statistics
#' @export
#' @useDynLib awrar
#' @examples
#' ##
#'
performance.stats <- function(obs,run,start,start.water.year="07-01"){

	# Check class
	if(!inherits(run,"awrar-run")) stop("class of run object is not awrar-run")
	if(!inherits(start,"Date"))
	{
		err <- try(start <- as.Date(start),silent=TRUE)
		if(inherits(err,"try-error")) stop("Failed converting start to a date")
	}

	# Check start water year
	tmp <- paste("2000",start.water.year,sep="-")
	err <- try(as.Date(tmp),silent=TRUE)
	if(inherits(err,"try-error")) stop("Wrong format for start.water.year. Should be MM-DD")
	

	# inputs data
	sd <- setdiff(colnames(obs),c("day","flow"))
	if(length(sd)!=0) 
			stop("wrong columns in obs (should be day and flow)\n",sprintf("\tcol %s not valid\n",sd))

	# flow validity
	obs$flow[!is.finite(obs$flow)] <- -999
	ii <- which(obs$flow>=0)
	if(length(ii)<365*5) stop("less than one 5 years of obs data, can't compute")
	obs <- obs[min(ii):max(ii),]

	# simulated data
	sim <- run$outflow
	nval <- length(sim)
	day <- seq(start,by="day",length.out=nval)

	# matching date
	i1 <- match(day,obs$day); i1 <- i1[!is.na(i1)]
	i2 <- match(obs$day,day); i2 <- i2[!is.na(i2)]	
	sim <- sim[i2]
	day <- day[i2]
	obs <- obs[i1,]
	nval <- nrow(obs)

	# Water years
	water.year <- cumsum(as.integer(format(day,
			"%m-%d")==start.water.year))-1+as.integer(format(day[1],"%Y"))
	water.year <- as.Date(paste(water.year,start.water.year,sep="-"))

	# month
	month <- as.Date(format(day,"%y-%m-01"))

	# Initialise
	stats <- data.frame(
		bias = NA,
		max.yearly.bias = NA,
		nse = NA,
		nse.monthly = NA,
		nse.log = NA)

	# Compute perfs
	ii <- which(obs$flow>=0)

	stats$bias <- (sum(sim[ii])-sum(obs$flow[ii]))/sum(obs$flow[ii])
	stats$nse <- 1-sum((sim[ii]-obs$flow[ii])^2)/sum((mean(obs$flow[ii])-obs$flow[ii])^2)

	flog <- function(v) log(v+1e-4)
	stats$nse.log <- 1-sum((flog(sim[ii])-flog(obs$flow[ii]))^2)/sum((mean(flog(obs$flow[ii]))-flog(obs$flow[ii]))^2)

	# Max bias
	yy <- aggregate(cbind(rep(1,length(ii)),sim[ii],obs$flow[ii]),list(water.year=water.year[ii]),
							function(v) sum(v))
	colnames(yy) <- c("water.year","nval","sim","obs")
	yy$sim <- yy$sim/yy$nval
	yy$obs <- yy$obs/yy$nval

	yy$bias <- (yy$sim-yy$obs)/yy$obs
	kk <- which(abs(yy$bias)==max(abs(yy$bias),na.rm=TRUE))
	if(length(kk)>1) kk <- kk[1]
	stats$max.yearly.bias <- yy$bias[kk]

	mm <- aggregate(cbind(rep(1,length(ii)),sim[ii],obs$flow[ii]),list(water.year=month[ii]),
							function(v) sum(v))
	colnames(mm) <- c("month","nval","sim","obs")
	mm$sim <- mm$sim/mm$nval
	mm$obs <- mm$obs/mm$nval
	stats$nse.monthly <- 1-sum((mm$sim-mm$obs)^2)/sum((mean(mm$obs)-mm$obs)^2)

	class(stats) <- "awrar-stats"
	return(list(stats=stats,monthly=mm,yearly=yy,water.year=water.year));
}

