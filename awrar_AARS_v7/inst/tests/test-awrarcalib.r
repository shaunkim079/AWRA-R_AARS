context("awrarcalib.run")

if(!skip.awrarcalib.tests)
{

	#---------------------- TESTS --------------------------------------------------

	test_that("full run", {

	  # Model inputs
	  inputs <- test.data1$inputs
	  config.awrar <- get.default.config()
	  dt <- config.awrar[7]
	  config.awrar[1] <- 1
	  config.awrar[2] <- 1
	  config.awrar[5] <- 1
	  config.awrar[11] <- 0.6 # Second parameter of the Area/Volume relationship
	  parameters <- unlist(test.data1$parameters)
	  parameters[7] <- 1

	  # Run model
	  out <- awrar.run(parameters,config.awrar,inputs)

	  # Run awrarcalib
	  config<-list(rrmod="AWRALSCALE",
				config.awrar=config.awrar,
				has.flood =TRUE,
				river.length=1e5,
				flood.ksat=parameters[2])

	  pu <- awrarcalib.parutils(config)
	  pars <- pu$modpardefault
      pars[c(1:5,8)] <- parameters[c(1,3:6,7)]
	  pars[2] <- parameters[3]/parameters[4]
	  pars[6] <- 0

 	  ninflow  <- ncol(inputs) - get.constant("NINPUTSNONROUTING")
	  data <- list(inputs=inputs,config=config)

	  out2 <- awrarcalib.run(pars,data)	
	  delta <- max(abs(out$outflow-out2$outflow))
	  expect_true(delta<1e-15,info="delta on simulations")

	  ppars <- pu$parcalib2parC(pars)
	  delta <- max(abs(parameters-ppars))
	  expect_true(delta<1e-15,info="delta on parcalib2parC")

	})

	test_that("with gr4j", {

	  data <- test.data3$data 
 	  parameters <- test.data3$parameters
	  err <- try(out <- awrarcalib.run(parameters,data),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))

	})

	test_that("with reservoir and carry over output", {

	  data <- awrarcalib.testdata1$data 
	  data$config$add.reservoir.carryover <- FALSE
 	  parameters <- awrarcalib.testdata1$parameters	  

	  err <- try(out <- awrarcalib.run(parameters,data),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))
	})


	test_that("calibration", {

      library(hydrodiy)

	  # Model inputs
	  outputs <- test.data1$outputs 
	  inputs <- test.data1$inputs
	  config.awrar <- get.default.config()
	  config.awrar[1] <- 1
	  config.awrar[2] <- 1
	  config.awrar[11] <- 0.6 # Second parameter of the Area/Volume relationship
 	  parameters <- unlist(test.data1$parameters)

	  # Calib data
	  config<-list(rrmod="AWRALSCALE",
				config.awrar=config.awrar,
				has.flood =TRUE,
				river.length=1e5,
				flood.ksat=parameters[2])
	  data <- list(obs=outputs,inputs=inputs,config=config)

	  # Preparation of calibr
	  mw <- awrarcalib.parutils(config)
	  mlib <- mw$modparlib
	  data$config$modparlib <- mlib[1:50,]
	  bd <- mw$modparbounds
    bd[,2] <- bd[,2]+1e-3
	  data$config$modparbounds <- bd			
	  control <- getoptimisecontrol(data$obs,nper=2,warmup=0)
	  control$optimiser.bl <- mw$true2trans(bd[,1])
	  control$optimiser.bu <- mw$true2trans(bd[,2])+1e-3

	  bc.bias<-function(obs,sim,validata,nper)
		{
            lambda <- 0.5
			mu <- 1
			E <- mean( (extended.boxcox(obs[validata],lambda)-extended.boxcox(sim[validata],lambda))^2 )
			O <- mean(obs[validata])
			if(O==0) O=1
			M <- mean(sim[validata])
			F<- -E*(1+abs((M-O)/O)^mu)
		}
      objfunc <- bc.bias

	  model.wrapper <- list(
			run = awrarcalib.run,
			parutils=awrarcalib.parutils,
			initialise=awrarcalib.initialise
		)
	  
	 optimiser <- rbase.optimiser	  


	  # Calibration
#      cal <- optimjob(control,data,objfunc,model.wrapper,optimiser=optimiser)
#		
#      pars <- t(summary(cal)$modpar)
#	  out.ini <- awrarcalib.run(test.data1$parameters,data)
#	  out.cal <- awrarcalib.run(pars,data)
#	
#      st.ini <- out.ini$states.nonrouting
#      st.cal <- out.cal$states.nonrouting

	  #matplot(cbind(out.ini$outflow,out.cal$outflow),type="l")
	  #delta <- max(abs(out.ini$outflow-out.cal$outflow))
	  #expect_true(delta<1e-15)
	})


} else 
{
	cat("\n\n..awrarcalib.run NOT tested ..\n\n")
}
