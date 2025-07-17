#' Check the name of an object attributes
#'
#' @param obj A S3 list object
#' @param expected.names Vector of strings listing the names of the attributes or arguments that should be present in \code{obj}
#' @param undesirable.names Vector of strings listing the names of the attributes or arguments that should not be present in \code{obj}
#' @param objname Object name to be mentioned in the output message
#' @return A list object containing two elements: \code{passed} indicates the test is passed, \code{message} gives some info
#' @export
#' @examples
#' o <- list(u="AADSASD",v=rnorm(100))
#' ck1 <- checknames(o,c("u","v"))
#' ck2 <- checknames(o,c("u","w"))
#'
checknames <- function(obj,expected.names,undesirable.names=NULL,objname=""){
	out <-list(passed=TRUE,message="")

	# Get names
	if(class(obj)!="function") nm <- attributes(obj)$names
	else nm <- names(formals(obj))

	missing <- expected.names[!expected.names%in%nm]

	undesired <- NULL
	if(!is.null(undesirable.names)) undesired <- undesirable.names[undesirable.names%in%nm]

	if(class(obj)!="function") type <- "attributes"
	else type<- "arguments"

	if(length(missing)>0|length(undesired)>0) {
		out$passed<-FALSE;out$message <- 
			paste("\nHumm, apparently I have some problems with the",type,"of <",objname,">:\n\tMissing = ",
			paste(missing,collapse=" "),"\n\tUndesired = ",paste(undesired,collapse=" "),"\n")
	}	
	return(out)
}

