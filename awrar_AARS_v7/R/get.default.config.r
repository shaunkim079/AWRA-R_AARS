#' Get a default config parameter for \link{awrar.run}
#'
#' The default config only activates routing and set the times step to daily
#'
#' @return a config vector (see \link{awrar.run}) :
#' @export
#' @useDynLib awrar
#' @examples
#' cf <- get.default.config()
#'
get.default.config <- function(){
  nconfig <- get.constant("NCONFIG")
  config <- rep(0,nconfig)
  config[1] <- 1
  config[7] <- 86400
  
  config[13] <- 1 # Anabranch flow exponent 1
  config[15] <- 1 # Anabranch flow exponent 2
  
  config[15] <- 50000 # floodplain length
  config[15] <- 40 # river alpha
  config[15] <- 0.9 # river beta
  
  names(config) <- c("use.routing","use.flood","use.monod",
      "use.reservoir","use.ungauged","use.anabranch","time.step",
      "river.area1","river.area2","flood.area1","flood.area2",
      "anabranch.top1","anabranch.top2","anabranch.bottom1",
      "anabranch.bottom2","floodplain.length","river.alpha","river.beta")
  
	return(config);
}
