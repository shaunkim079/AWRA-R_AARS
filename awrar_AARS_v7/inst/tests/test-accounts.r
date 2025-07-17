context("get.accounts")

test_that("dummy test", {
 # Awrar run
 inputs <- test.data1$inputs
 config <- get.default.config()
 dt <- config[7]
 config[1] <- 1
 config[2] <- 1
 config[11] <- 0.6 # Second parameter of the Area/Volume relationship
 parameters <- unlist(test.data1$parameters)
 parameters[1] <- 1/50 				# Return flow coefficient
 parameters[5] <- 50 					# Overbank flow threshold
 river.run <- awrar.run(parameters,config,inputs)
             nval <- length(river.run$outflow)
 zero <- rep(0,nval)
 data <- data.frame(ofs=zero,
			swdiversion=zero,
			gwdiversion=zero,
			floodharvest=zero,
			returnflow=zero,
			evapofs=zero,
			rainfallofs=zero,
			evapcrop=zero,
			rainfallcrop=zero,
			applicationcrop=zero,
			gwlossofs=zero)
	irrig.run <- data2irrigation.run(data)

  err <- try(accounts <- get.accounts(river.run,irrig.run,start="1970-01-01"),silent=TRUE)
  expect_true(!inherits(err,"try-error"))
})
