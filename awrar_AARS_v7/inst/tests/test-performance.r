context("performance.stats")

# Model run
inputs <- test.data1$inputs
config <- get.default.config()
dt <- config[7]
config[1] <- 1
config[2] <- 1
config[11] <- 0.6 # Second parameter of the Area/Volume relationship
parameters <- unlist(test.data1$parameters)
parameters[1] <- 1/50 				# Return flow coefficient
parameters[5] <- 50 					# Overbank flow threshold
out <- awrar.run(parameters,config,inputs)
start <- as.Date("1970-01-01")
nval <- length(out$outflow)
day <- seq(start,by="day",length.out=nval)

test_that("bias", {
  obs1 <- data.frame(day=day,flow=out$outflow*1/0.9)	
  err <- try(stats <- performance.stats(obs1,out,start),silent=TRUE)
  expect_true(!inherits(err,"try-error"))
  if(!inherits(err,"try-error"))
  {
		delta <- abs(stats$stats$bias+0.1)
		expect_true(delta<1e-16,info="test mass balance - A")
		delta <- abs(stats$stats$max.yearly.bias+0.1)
		expect_true(delta<1e-16,info="test mass balance - B")
  }
})

test_that("nse", {
  set.seed(1)
  err <- (rlnorm(nval)-1)/10+1
  obs2 <- data.frame(day=day,flow=out$outflow*err)
  ii <- which(obs2>=0)
  lobs2 <- log(pmax(0,obs2$flow)+1e-4)
  lsim <- log(pmax(0,out$outflow)+1e-4)

  nse <- 1-sum((obs2$flow-out$outflow)^2)/sum((obs2$flow-mean(obs2$flow))^2)
  nse.log <- 1-sum((lobs2-lsim)^2)/sum((lobs2-mean(lobs2))^2)
	
  err <- try(stats <- performance.stats(obs2,out,start),silent=TRUE)
  expect_true(!inherits(err,"try-error"))
  if(!inherits(err,"try-error"))
  {
		expect_true(stats$stats$nse==nse,info="test nse - A")
		expect_true(stats$stats$nse.log==nse.log,info="test nse - B")
  }
})

test_that("date", {
  set.seed(1)
  err <- rlnorm(nval)
  obs2 <- data.frame(day=day,flow=out$outflow*err)
  obs2 <- obs2[1:5000,]

})

