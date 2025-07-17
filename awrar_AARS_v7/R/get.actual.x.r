#' Get the actual x Muskingum parameter for given K and dt 
#'
#' The actual x value is determined as
#' actual.x = min(dt/2/k,max(1-dt/2/k))	
#'
#' @param x Proposed x value
#' @param K Value of the K parameter (seconds)
#' @param dt Time step duration (seconds)
#' @return actual x value avoiding negative outflows
#' @export
#' @useDynLib awrar
#' @examples
#' ###
#'
get.actual.x <- function(x,K,dt){

	# inputs / outputs
	ierr <- as.integer(0)
	dt <- as.double(dt)
	K <- as.double(K)
	x <- as.double(x)
	actualx <- as.double(x)	

	# Run the C code
	#void get_actual_x(int * ierr,
	#	double * dt,double * x,double * K, double * actualx)
	out <- .C("get_actual_x",ierr=ierr,dt=dt,x=x,K=K,actualx=actualx)

  	if(out$ierr>0) stop(sprintf("\nget.actual.x error - C code returned error %d\n",out$ierr))

	return(out$actualx);
}

