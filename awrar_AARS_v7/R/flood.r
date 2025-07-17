#' Flood routine used to test the AWRAR flood component
#'
#' Routine coded by Justin Hughes in Fortran
#'
#' @param Qin	Floodplain inflow
#' @param P		Floodplain rainfall
#' @param E		Floodplain evaporation
#' @param paramj	Floodplain model parameters
#'	\itemize{
#'		\item paramj[1] : Overbank flow threshold
#'		\item paramj[2] : multiplier in overbank flow model
#'		\item paramj[3] : exponent in overbank flow model
#'  }
#' @param floodAlpha Multiplier in the relationship flooded area = a . flooded volume	
#' @param Ksat Floodplain infiltration
#' @return A list object with the following structure:
#'	\itemize{
#'		\item outflow : Flow at the downstream end of the reach
#'		\item states.nonrouting : model states not related to routing  (see awrar_runtimestep.c)
#'		\item states.routing : model states related to routing (see awrar_runtimestep.c)
#'  }
#' @export
#' @useDynLib awrar
#' @examples
#'	## Inputs
#' 	Qin <- c(0,100,rep(0,4))
#' 	n <- length(Qin)
#' 	P <- rexp(n)*1e-3/86400
#' 	E <- (P*0+5)*1e-3/86400
#' 	
#'	## parameters
#' 	paramj <- c(90,1,0.5)
#' 	floodAlpha <- 1/2
#' 	Ksat <- 1e-2
#' 	
#'	## model
#' 	out <- flood(Qin,P,E,paramj,floodAlpha,Ksat)
#'	matplot(out,type="o")
#'
flood <- function(Qin,P,E,paramj,floodAlpha,Ksat){

	# inputs
	Qin 	<- as.double(Qin)
	P 	<- as.double(P)
	E 	<- as.double(E)
	xx 		<- as.integer(length(Qin))
	if(length(P)!=xx) stop("length(P)!=xx")
	if(length(E)!=xx) stop("length(E)!=xx")

	paramj 	<- as.double(paramj)
	if(length(paramj)!=3) stop("length(paramj)!=3")

	floodAlpha <- as.double(floodAlpha)
	Ksat <- as.double(Ksat)

	# outputs
	Qupdate <- as.double(rep(0,xx))
	Qx		<- as.double(rep(0,xx))
	Qr		<- as.double(rep(0,xx))
	Vfp		<- as.double(rep(0,xx))
	a1		<- as.double(rep(0,xx))

	# Run the Fortran code
#       SUBROUTINE flood(xx,Vfp,Qin,Qx,Qupdate,Qr,a1,paramj,floodAlpha, 
#     1 			P, E, Ksat) 
	out <- .Fortran("flood",
				xx=xx,Vfp=Vfp,Qin=Qin,Qx=Qx,
				Qupdate=Qupdate,Qr=Qr,a1=a1,
				paramj=paramj,
				floodAlpha=floodAlpha,
				P=P,E=E,Ksat=Ksat)
	
	outputs <- data.frame(Qupdate=out$Qupdate,
			Qx=out$Qx,
			Qr=out$Qr,
			Vfp=out$Vfp,
			a1=out$a1)

	return(outputs)
}

