#' Convert irrigation data to the \link{irrigation.run} format
#'
#' The conversion is required to compute the water accounts with the \link{get.accounts} function.
#'
#' @param data Data frame containing irrigation model outputs with the following column names :
#'  \itemize{
#'    \item ofs
#'    \item swdiversion
#'    \item gwdiversion
#'    \item floodharvest
#'    \item returnflow
#'    \item evapofs
#'    \item rainfallofs
#'    \item evapcrop
#'    \item rainfallcrop
#'    \item applicationcrop
#'    \item gwlossofs
#'  }
#' @return A list object with the structure detailed in \link{irrigation.run}
#' @export
#' @useDynLib awrar
#' @examples
#' # dummy data set
#' zero <- rep(0,5000)
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
#'
#'	out <- data2irrigation.run(data)
#'
data2irrigation.run <- function(data){

  	if(is.null(dim(data))) stop("data should have at least 2 dimensions")
	
	sd <- setdiff(colnames(data),c("ofs",
			"swdiversion",
			"gwdiversion",
			"floodharvest",
			"returnflow",
			"evapofs",
			"rainfallofs",
			"evapcrop",
			"rainfallcrop",
			"applicationcrop",
			"gwlossofs"))

	if(length(sd)!=0) 
			stop("wrong columns in links\n",sprintf("\tcol %s not valid\n",sd))

	out <- list(diversion= data$swdiversion,
		inputs 	= 		NA,
		parameters = 	NA,
		config 	= 		NA,
		states	= data)

	colnames(out$states) <- c("irrigation.ofs",
			"irrigation.swdiversion",
			"irrigation.gwdiversion",
			"irrigation.floodharvest",
			"irrigation.returnflow",
			"irrigation.evapofs",
			"irrigation.rainfallofs",
			"irrigation.evapcrop",
			"irrigation.rainfallcrop",
			"irrigation.applicationcrop",
			"irrigation.gwlossofs")

    class(out) <- "irrigation-run"

	return(out)
}

