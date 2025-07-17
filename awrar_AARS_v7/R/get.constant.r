#' Get a constant declared in the C header file
#'
#' The constants are used to define the dimensions of some vectors and arrays.
#'
#' @param constant.name Name of constant
#' @return constant value. NA if not found in the list of constants.
#' @export
#' @useDynLib awrar
#' @examples
#' get.constant("NPARROUTING")
#' get.constant("NPARROUTIN")
#'
get.constant <- function(constant.name){

	# inputs / outputs
	ierr <- as.integer(0)
	constant.name <- as.character(toupper(constant.name))
	nstr <- as.integer(nchar(constant.name))
	value <- as.integer(0)

	# Run the C code
	#void get_constant(int * ierr, char * constant_name,int * value)
	out <- .C("get_constant",ierr=ierr,nstr=nstr,constant_name=constant.name,value=value)

  	if(out$ierr>0) stop(sprintf("\nget.constant error - C code returned error %d\n",out$ierr))

	if(out$value==0) out$value=NA	
	return(out$value);
}

