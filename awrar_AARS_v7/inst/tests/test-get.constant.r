context("get.constant")

test_that("test constants values", {

	cst <- c("NINFLOWMAX",
		"NPARNONROUTING",
		"NINPUTSNONROUTING",
		"NSTATESNONROUTING",
		"NMAXSTRING",
		"NPARROUTING",
		"NINPUTSROUTING",
		"NSTATESROUTING",
		"NCONFIG",
		"NPARIRRIG",
		"NINPUTSIRRIG",
		"NSTATESIRRIG",
		"NCONFIGIRRIG")
	
	for(i in 1:length(cst))
	{
		value <- get.constant(cst[i])
		expect_true(!is.na(value),info=paste("constant",cst[i]))
	}	

	
})
