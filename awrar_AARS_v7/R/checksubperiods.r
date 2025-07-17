#' Check that a subperiod object is suitable for use
#'
#' @param subper A subperiod object (e.g. \link{subperiods})
#' @return A list object containing two elements: \code{passed} indicates the test is passed, \code{message} gives some info
checksubperiods <- function(subper){

	# Check attributes
	out <- checknames(subper,
			c("nper","nval","warmup","startend","pervalidata"),objname="subperiods")

	# Dimensions
	if(out$passed){
		if(ncol(subper$startend)!=2){
			out$passed<-FALSE
			out$message <- "\nStartend does not have 2 columns\n"
		}	
		nper <- subper$nper
		if(nrow(subper$startend)!=nper){
			out$passed<-FALSE
			out$message <- paste(out$message,
				"\nStartend does not have nper=",nper," rows\n")
		}	
		ii <- grep("0$",rownames(subper$startend),invert=TRUE)
		d <- subper$startend[ii,2]-subper$startend[ii,1]
		if(length(which(abs(diff(d))>2))>0){
			out$passed<-FALSE
			out$message <- paste(out$message,
				"\nDifferences in the duration of the periods\n")
		}	

		if(length(subper$pervalidata)!=nper){
			out$passed<-FALSE
			out$message <- paste(out$message,
				"\nPervalidata does not have a length equal to nper=",nper,"\n")
		}	
	}

	if(out$passed) out$message <-"All good, mate"
	return(out)
}

