#' Generate subperiods for a split-sample test exercise
#'
#' @param nval Number of time step
#' @param nper Number of subperiods
#' @param warmup Duration of the warmup period at the beginning of each sub-period
#' @param validata Indices of the vector where an objective function or a performance score can be computed (e.g. non missing values). 
#' @param addfullperiod Add a subperiod covering the whole period. Default is FALSE.
#' @export
#' @return A list object containing
#'		\itemize{
#'			\item startend : A matrix containing the index of start and end of subperiods
#'			\item pervalidata : A list object containing the indices of the valid data in each period (taking into account the warmup period)
#'		}
#' @examples
#' nval <- 1000
#' s<-subperiods(nval,3,100,1:nval)
#' s$startend
#'
subperiods <- function(nval,nper,warmup,validata,addfullperiod=FALSE){

	# Input checks
	if(nval<=warmup+1) stop("[0004] Not enough data to generate sub-periods")
	if(nval-warmup-1<nper) stop("[0005] Not enough data to generate sub-periods")
	if(length(which(nval-range(validata)<0))>0) stop("[0006] Wrong validata")
	if(!is.integer(validata)) stop("[0007] validata should contain integers")

	# Compute the indices of start and end of periods
	d <- (nval-warmup-1)/nper
	out <- list(warmup=warmup,nval=nval)
	end <- d*(1:nper)+warmup+1
	out$startend<-cbind(end-d-warmup,end)
	out$startend <- matrix(apply(out$startend,2,
			function(x) pmin(nval,pmax(1,round(x)))),nper,2)
	if(addfullperiod & nper>1){
		out$nper <- nper+1
		out$startend <- rbind(out$startend,c(1,nval))
	}else out$nper <- nper

	# Give names
	colnames(out$startend) <- c("start","end")

	last <- NULL; if(addfullperiod & nper>1) last<-"PER0"
	rownames(out$startend) <- c(paste("PER",1:nper,sep=""),last)

	# Compute the indices where the objective function will be evaluated
	out$pervalidata <- list()
	for(i in 1:nrow(out$startend))
	{
		iok <-which(validata>=out$startend[i,1]+warmup & validata<=out$startend[i,2])
		if(length(iok)==0){
			stop(paste("subperiods : No valid indices for the period starting from",
						out$startend[i,1],"and ending at",out$startend[i,2]))
		}
		out$pervalidata[[length(out$pervalidata)+1]] <- sort(validata[iok])
	}

	class(out)<-"subperiods"
	return(out)
}

