context("awrarirrig.run")

if(!skip.awrarirrig.tests)
{

	#---------------------- UTILITIES ----------------------------------------------

	# max absolute difference allowed between model output and expected response
	maxdiff <- 1e-9 

	ntest <- 100
	savetest <- function(parameters,inputs,config,expect)
	{
		d <- list(parameters=parameters,inputs=inputs,
				config=config,expect=expect)
		eval(parse(text=paste("test.data",ntest,"<-d",sep="")))
		path <- Sys.getenv("AWRAR_PACKAGE")
		ntest.txt <- sprintf("%3.3d",ntest)
		ft <- paste(path,"/tests/test.data",ntest.txt,".rda",sep="")
		eval(parse(text=paste("test.data",ntest.txt,"<-d",sep="")))
		eval(parse(text=paste("save(test.data",ntest.txt,",file=ft)",sep="")))	
		ntest <<- ntest + 1
	}

	#---------------------- TESTS --------------------------------------------------

	test_that("full run with irrigation", {

	  # Model inputs
	  inputs <- test.data1$inputs
	  config <- get.default.config()
	  dt <- config[7]
	  config[1] <- 1
	  config[2] <- 1
	  config[11] <- 0.6 # Second parameter of the Area/Volume relationship
	  parameters <- unlist(test.data1$parameters)
	  parameters[1] <- 1/50 				# Return flow coefficient
	  parameters[5] <- 50 					# Overbank flow threshold
	  parameters[6] <- 0.6  				# Overbank flow exponent

	  # Run awrar model
	  out <- awrar.run(parameters,config,inputs)

	  # Run awrarirrig model
	  parameters.irrig <- rep(0,16)
	  parameters.irrig[1:7] <- parameters[1:7]
	  parameters.irrig[14:16] <- parameters[8:10]

	  config.irrig <- c(config,0,rep(0,7)) # use_irrig=0 => no irrig model
	  inputs.irrig <- cbind(inputs[,1:11],matrix(0,nrow(inputs),4),inputs[,12])

	  err <- try(out.irrig <- awrarirrig.run(parameters.irrig,config.irrig,inputs.irrig),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))
	  if(!inherits(err,"try-error"))
	  {
		  delta1 <- max(abs(out$outflow-out.irrig$outflow))
		  delta2 <- max(abs(out$states.nonrouting-out.irrig$states.nonrouting))
		  delta3 <- max(abs(out$states.routing-out.irrig$states.routing))
		  delta <-max(delta1,delta2,delta3)
		  expect_true(delta<1e-10,info="delta test")	
	 	  savetest(parameters.irrig,inputs.irrig,config.irrig,out.irrig)
	  }

	  config.irrig <- c(config,1,rep(0,7)) # use_irrig=1 => irrig model
	  # but, all inputs set to 0 and original simul did not have irrig
	  err <- try(out.irrig <- awrarirrig.run(parameters.irrig,config.irrig,inputs.irrig),silent=TRUE)

	  expect_true(!inherits(err,"try-error"))
	  if(!inherits(err,"try-error"))
	  {
		  delta1 <- max(abs(out$outflow-out.irrig$outflow))
		  delta2 <- max(abs(out$states.nonrouting-out.irrig$states.nonrouting))
		  delta3 <- max(abs(out$states.routing-out.irrig$states.routing))
		  delta <-max(delta1,delta2,delta3)
		  expect_true(delta<1e-10,info="delta test 2")	
	 	  savetest(parameters.irrig,inputs.irrig,config.irrig,out.irrig)
	  }


	})

# Another test to go here written by Justin/Ang
# to make sure Irrigation module is working ok !
  
} else 
{
	cat("\n\n..awrarirrig.run NOT tested ..\n\n")
}
