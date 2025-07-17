context("flood")

test_that("test errors", {

	Qin <- c(0,100,rep(0,4))
	n <- length(Qin)
	P <- rexp(n)*1e-3/86400
	E <- (P*0+5)*1e-3/86400

	paramj <- c(90,1,0.5)
	floodAlpha <- 1/2
	Ksat <- 1e-2

	out <- flood(Qin,P,E,paramj,floodAlpha,Ksat)
	#matplot(out)
})
