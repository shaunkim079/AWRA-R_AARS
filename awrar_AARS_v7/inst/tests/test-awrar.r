context("awrar.run")

if(!skip.awrar.tests)
{

	#---------------------- UTILITIES ----------------------------------------------

	# max absolute difference allowed between model output and expected response
	maxdiff <- 1e-9 

	ntest <- 1
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


	# R implementation of the Muskingum model
	musk <- function(Qin,dt,K,x)
	{
		K <- max(K,dt/2)
		x <- get.actual.x(x,K,dt)
		D <- K*(1-x)+dt/2
		c0 <- -(K*x-dt/2)/D
		c1 <- (K*x+dt/2)/D
		c2 <- (K-K*x-dt/2)/D
		n <- length(Qin)
		Qout <- 0*Qin
		Qprev <- c(0,0)

		for(i in 1:n) 
		{
			Qout[i] <- c0*Qin[i]+c1*Qprev[1]+c2*Qprev[2]
	if(Qout[i]<0) browser()
			Qprev 	<- c(Qin[i],Qout[i])
		}

		return(Qout)
	}

	# R implementation of flood model
	flood <- function(Qup,P,E,Ksat,r,beta,gamma,Q0,dt=86400)
	{
		n <- length(Qup)
		V <- 0
		if(length(P)==1) P=rep(0,n)
		if(length(E)==1) E=rep(0,n)

		zero <- Qup*0
		out <- data.frame(V=zero,A=zero,
					of=pmin(pmax(Qup-Q0,0),pmax(Qup-Q0,0)^gamma),
					rf=zero,final=zero,
					rainfall=zero,evap=zero,gw=zero)

		for(i in 1:n)
		{
			V <- V + out$of[i]*dt
			out$A[i] <- V*beta 

			out$rainfall[i] <- out$A[i]*P[i]*1e-3/86400
			out$evap[i] <- out$A[i]*E[i]*1e-3/86400
			out$gw[i] <-  out$A[i]*Ksat
			V <- pmax(0,V + (out$rainfall[i]-out$evap[i]-out$gw[i])*dt)

			out$rf[i] <- V * r/dt
			V <- V - out$rf[i]*dt
			out$V[i] <- V
			out$final[i] <- Qup[i]-out$of[i]+out$rf[i]	
		}
		return(out)
	}


	#---------------------- TESTS --------------------------------------------------

	test_that("lag - single inflow", {

	  Qin <- c(0,100,rep(0,20))
	  n <- length(Qin)
	  ni <- get.constant("NINPUTSNONROUTING")
	  inputs <- cbind(t(matrix(0,ni,n)),Qin)

	  config <- get.default.config()
	  dt <- config[7]

	  K <- 0
	  x <- 0
	  lag <- dt
	  parameters <- c(0,0,0,0,0,0,0,lag,K,x)
	  expect <- c(0,0,100,rep(0,n-3))
	  out <- awrar.run(parameters,config,inputs)
	  expect_equal(out$outflow,expect,info=sprintf("test %d single inflow #1",ntest))
	  savetest(parameters,inputs,config,expect)

	  parameters[8] <- dt*4
	  out <- awrar.run(parameters,config,inputs)
	  expect <- c(0,rep(0,4),100,rep(0,n-6))
	  expect_equal(out$outflow,expect,info=sprintf("test %d single inflow #2",ntest))
	  savetest(parameters,inputs,config,expect)

	  parameters[8] <- dt*1.6
	  out <- awrar.run(parameters,config,inputs)
	  expect <- c(0,0,40,60,rep(0,n-4))
	  expect_equal(out$outflow,expect,info=sprintf("test %d single inflow #3",ntest))
	  savetest(parameters,inputs,config,expect)

	})

	test_that("lag - triple inflow", {

	  Qin1 <- c(0,100,rep(0,20))
	  n <- length(Qin1)
	  Qin2 <- c(0,0,90,rep(0,n-3))
	  Qin3 <- c(0,0,0,80,rep(0,n-4))
	  Qs <- Qin1+Qin2+Qin3

	  ni <- get.constant("NINPUTSNONROUTING")
	  inputs <- cbind(t(matrix(0,ni,n)),Qin1,Qin2,Qin3)

	  config <- get.default.config()
	  dt <- config[7]

	  K <- 0
	  x <- 0
	  lag1 <- dt
	  lag2 <- dt
	  lag3 <- dt
	  parameters <- c(0,0,0,0,0,0,0,lag1,K,x,lag2,K,x,lag3,K,x)

	  out <- awrar.run(parameters,config,inputs)
	  expect <- c(0,Qs[1:(n-1)])
	  expect_equal(out$outflow,expect,info=sprintf("test %d lag triple inflows #1",ntest))
	  savetest(parameters,inputs,config,expect)

	  lag1 <- dt*4
	  lag2 <- dt*3
	  lag3 <- dt
	  parameters <- c(0,0,0,0,0,0,0,lag1,K,x,lag2,K,x,lag3,K,x)
	  out <- awrar.run(parameters,config,inputs)
	  d1 <- c(rep(0,4),Qin1[1:(length(Qin1)-4)]) 
	  d2 <- c(rep(0,3),Qin2[1:(length(Qin1)-3)])  
	  d3 <- c(rep(0,1),Qin3[1:(length(Qin1)-1)])
	  expect <- d1+d2+d3
	  expect_equal(out$outflow,expect,info=sprintf("test %d lag triple inflows #2",ntest))
	  savetest(parameters,inputs,config,expect)

	  lag1 <- dt*0.5
	  lag2 <- dt*0.8
	  lag3 <- dt*1.2
	  parameters <- c(0,0,0,0,0,0,0,lag1,K,x,lag2,K,x,lag3,K,x)
	  out <- awrar.run(parameters,config,inputs)
	  d1 <- 0.5*Qin1+0.5*c(rep(0,1),Qin1[1:(length(Qin1)-1)])  
	  d2 <- 0.2*Qin2 + 0.8*c(rep(0,1),Qin2[1:(length(Qin1)-1)])
	  d3 <- 0.8*c(rep(0,1),Qin3[1:(length(Qin1)-1)])+ 0.2*c(rep(0,2),Qin3[1:(length(Qin1)-2)])
	  expect <- d1+d2+d3
	  expect_equal(out$outflow,expect,info=sprintf("test %d lag triple inflows #3",ntest))
	  savetest(parameters,inputs,config,expect)

	})

	test_that("muskingum - single inflow", {

	  Qin <- c(0,100,rep(0,4))
	  n <- length(Qin)
	  ni <- get.constant("NINPUTSNONROUTING")
	  inputs <- cbind(t(matrix(0,ni,n)),Qin)

	  config <- get.default.config()
	  dt <- config[7]
	  
	  K <- dt
	  x <- 0.1
	  lag <- 0
	  parameters <- c(0,0,0,0,0,0,0,lag,K,x)

	  out <- awrar.run(parameters,config,inputs)
	  expect <- musk(Qin,dt,K,x)
	  expect_equal(out$outflow,expect,info=sprintf("test %d single inflow",ntest))
	  savetest(parameters,inputs,config,expect)

	})
	
	test_that("muskingum - multiple inflow", {

	  Qin1 <- c(0,100,rep(0,20))
	  n <- length(Qin1)
	  Qin2 <- c(0,0,90,rep(0,n-3))
	  Qin3 <- c(0,0,0,80,rep(0,n-4))

	  ni <- get.constant("NINPUTSNONROUTING")
	  inputs <- cbind(t(matrix(0,ni,n)),Qin1,Qin2,Qin3)

	  config <- get.default.config()
	  dt <- config[7]

	  K <- dt
	  x <- 0.1
	  lag <- 0
	  parameters <- c(0,0,0,0,0,0,0,lag,K,x,lag,K,x,lag,K,x)

	  out <- awrar.run(parameters,config,inputs)
	  expect <- musk(Qin1,dt,K,x)+musk(Qin2,dt,K,x)+musk(Qin3,dt,K,x)
	  delta <- abs(max((out$outflow-expect)))
	  expect_equal(out$outflow,expect,info=sprintf("test %d muskingum - multiple inflow",ntest))
	  savetest(parameters,inputs,config,expect)
	})

	test_that("realistic routing test, single inflow", {
	  
	  inputs <- test.data2$inputs
	  inflow <- inputs[,12:ncol(inputs)]
	  ninflow <- ncol(inflow)
	  parameters <- test.data2$parameters
	  config <- test.data2$config

	  
	  inflow <- inflow[,7]
	  inputs <- cbind(inputs[,1:11],inflow)
	  ninflow <- 1

	  p <- matrix(parameters[8:length(parameters)],3,10)
	  dt <- config[7]
	  K <- p[2,]
	  x <- p[3,]
	  parameters <- c(parameters[1:7],p[1,7],K[7],x[7])

	  err <- try(out <- awrar.run(parameters,config,inputs),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))
	  if(!inherits(err,"try-error"))
	  {
		delta <- abs(sum(inflow)-sum(out$outflow))
	   	expect_true(delta<maxdiff,info=sprintf("realistic routing mass balance"))

		expect <- musk(inflow,dt,K[7],x[7])
		delta <- abs(max((out$outflow-expect)))
	  	expect_true(delta<maxdiff,info=sprintf("test %d realistic routing output",ntest))
		savetest(parameters,inputs,config,expect)
	  }  
	})

	test_that("realistic routing test, 10 inflows", {
	  
	  inputs <- test.data2$inputs
	  inflow <- inputs[,12:ncol(inputs)]
	  ninflow <- ncol(inflow)
	  parameters <- test.data2$parameters
	  config <- test.data2$config
	  dt <- config[7]
	  K <- parameters[seq(9,length(parameters),by=3)]
	  x <- parameters[seq(10,length(parameters),by=3)]

	  err <- try(out <- awrar.run(parameters,config,inputs),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))
	  expect <- 
	  if(!inherits(err,"try-error"))
	  {
		delta <- abs(sum(inflow)-sum(out$outflow))
	   	expect_true(delta<maxdiff,info=sprintf("realistic routing mass balance"))

	 	routed <- 0*inflow
		for(i in 1:ninflow) routed[,i]<- musk(inflow[,i],dt,K[i],x[i])
		expect <- rowSums(routed)
		delta <- abs(max((out$outflow-expect)))
	  	expect_true(delta<maxdiff,info=sprintf("test %d realistic routing output",ntest))
		savetest(parameters,inputs,config,expect)
	  }  
	})


	test_that("flood test 0", {
	  
	  # Dimensions
	  ninflow <- 1
	  nval <- 200

	  # Model inputs
	  set.seed(11)
	  u <- arima.sim(n=nval*ninflow,model=list(ar=0.99))
	  inflow <- matrix(exp(sign(u)*abs(u)^0.5),nval,ninflow)
	  ni <- get.constant("NINPUTSNONROUTING")
	  inputs <- cbind(matrix(0,nval,ni),inflow)
	  
	  # Config
	  config <- get.default.config()
	  config[1] <- 0 # no routing
	  config[2] <- 1 # use flood model
	  dt <- config[7]

	  # Parameters
	  parameters <- rep(0,10)
	  parameters[1] <- 0 # no return flow
	  parameters[2] <- 0 # no Ksat
	  Qt <-  median(inflow)
	  parameters[5] <- Qt # threshold
	  parameters[6] <- 0.1  # exponent
	  parameters[7] <- 1

	  err <- try(out <- awrar.run(parameters,config,inputs),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))
	  if(!inherits(err,"try-error"))
	  {
		  out2 <- flood(inflow,P=0,E=0,Ksat=parameters[2],r=parameters[1],
						beta=config[11],gamma=parameters[6],Q0=Qt,dt=86400)
		  delta <- max(abs(out$states.nonrouting[,2]-out2$of))
		  expect_true(delta<maxdiff,info="overbank flow")

		  expect <- out2$final
		  delta <- max(abs(out$outflow-expect))
		  expect_true(delta<1e-20,info=sprintf("test %d flood 0 #1",ntest))
	  	  savetest(parameters,inputs,config,expect)
	  }

	  parameters[1] <- 1/10 # return flow
	  parameters[2] <- 20*1e-3/dt # Ksat
	  config[11] <- 1/2 # floodplain beta
	  err <- try(out <- awrar.run(parameters,config,inputs),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))

	  if(!inherits(err,"try-error"))
	  {
		  out2 <- flood(inflow,P=0,E=0,Ksat=parameters[2],r=parameters[1],
						beta=config[11],gamma=parameters[6],Q0=Qt,dt=86400)
		  delta <- max(abs(out$states.nonrouting[,3]-out2$V))
		  expect_true(delta<maxdiff,info="flood volume")

		  expect <- out2$final
		  delta <- max(abs(out$outflow-expect))
		  expect_true(delta<maxdiff,info=sprintf("test %d flood 0 #2",ntest))
	  	  savetest(parameters,inputs,config,expect)
	  }
	})

	test_that("test monod", {
	  
	  # Model inputs
	  inputs <- test.data1$inputs
	  config <- get.default.config()
	  config[1] <- 0
	  config[3] <- 1
	  parameters <- rep(0,10)
	  parameters[3] <- 0.5
	  parameters[4] <- 10

	  err <- try(out <- awrar.run(parameters,config,inputs),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))
	  if(!inherits(err,"try-error"))
	  {
		loss <- parameters[3]*inputs[,12]/(inputs[,12]+parameters[4])
		expect <- inputs[,12]-loss
		delta <- max(abs(out$outflow-expect))
		expect_true(delta<maxdiff,info=sprintf("test %d - monod",ntest))
	  	savetest(parameters,inputs,config,expect)
	  }
	})

	test_that("flood test 2", {
	  
	  # Model inputs
	  inputs <- test.data1$inputs
	  inputs[,1:4] <- 0
	  inflow <- inputs[,12]

	  config <- get.default.config()
	  dt <- config[7]
	  config[1] <- 0
	  config[2] <- 1
	  config[3] <- 0
	  config[11] <- 0.6
	  parameters <- unlist(test.data1$parameters)

	  parameters[1] <- 1/50 	# -> to 1/s
	  parameters[5] <- parameters[5]/86.4 		# -> to m3/s
    
    parameters[6] <- 0.5 # overbank flow exponent
    
	  err <- try(out <- awrar.run(parameters,config,inputs),silent=TRUE)
	  expect_true(!inherits(err,"try-error"))
	  if(!inherits(err,"try-error"))
	  {
		out2 <- flood(inflow,P=inputs[,3],E=inputs[,4],
					Ksat=parameters[2],r=parameters[1],beta=config[11],
					gamma=parameters[6],Q0=parameters[5])

		ov <- out$states.nonrouting[,2]
		expect <- out2$of
		delta <- max(abs(ov- expect))
		expect_true(delta<maxdiff,info="check overbank flow")

		a <- out$states.nonrouting[,4]
		expect <- out2$A
		delta <- max(abs(a-expect))
		expect_true(delta<maxdiff*1e3,info="check floodplain area")

		v <- out$states.nonrouting[,3]
		expect <- out2$V
		delta <- max(abs(v- expect))
		expect_true(delta<maxdiff*1e3,info="check floodplain volume")

		rr <- out$states.nonrouting[,5]
		expect <- out2$rf
		delta <- max(abs(rr- expect))
		expect_true(delta<maxdiff,info="check return flow")

		gw <- out$states.nonrouting[,10]
		expect <- out2$gw
		delta <- max(abs(gw- expect))
		expect_true(delta<maxdiff,info="check floodplain gw loss")

		expect <- out2$final
		delta <- max(abs(out$outflow-expect))
		expect_true(delta<maxdiff,info="outflow test")
	  	savetest(parameters,inputs,config,expect)
	  }
	})

	test_that("full run", {
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
	  parameters[6] <- 0.5 					# Overbank flow exponent

	  # Run model
	  out <- awrar.run(parameters,config,inputs)
	  err <- try(out <- awrar.run(parameters,config,inputs),silent=TRUE)

	  expect_true(!inherits(err,"try-error"))
	  if(!inherits(err,"try-error"))
	  {
		  expect <- out$outflow
	 	   savetest(parameters,inputs,config,expect)
	  }
	})

	test_that("diversion - single inflow", {

	  ni <- get.constant("NINPUTSNONROUTING")
	  n <- 20
	  Qin <- c(100,rep(0,n-1))
	  inputs <- cbind(matrix(0,n,ni),Qin)
	  div <- rep(50,n)
	  inputs[,5] <- div
	  config <- get.default.config()
	  parameters <- rep(0,10)

	  out <- awrar.run(parameters,config,inputs)
	  expect <- pmax(0,inputs[,12]-inputs[,5])
	  expect_equal(out$outflow,expect,info=sprintf("test %d diversion",ntest))
	  savetest(parameters,inputs,config,expect)

	})
	
	test_that("scaling factor - single inflow", {

	  ni <- get.constant("NINPUTSNONROUTING")
	  n <- 20
	  Qin <- c(100,rep(0,n-1))
	  inputs <- cbind(matrix(0,n,ni),Qin)
	  runoff <- rep(50,n)
	  inputs[,8] <- runoff
	  config <- get.default.config()
	  config[5] <- 1
	  parameters <- rep(0,10)
	  parameters[7] <- 0.6

	  out <- awrar.run(parameters,config,inputs)
	  expect <- inputs[,12]+parameters[7]*inputs[,8]
	  expect_equal(out$outflow,expect,info=sprintf("test %d scaling factor",ntest))
	  savetest(parameters,inputs,config,expect)

	})
	
	test_that("reservoir - single inflow", {

	  ni <- get.constant("NINPUTSNONROUTING")
	  n <- 20
	  Qin <- rep(100,n)
	  inputs <- cbind(matrix(0,n,ni),Qin)
	  config <- get.default.config()
	  config[4] <- 1
	  parameters <- rep(0,10)
	  dt <- config[7]
    vol <- 10+seq(0,by=10*dt,length.out=n)
    inputs[,10] <- vol
    rainfall <- rep(10,n)
    inputs[,1] <- rainfall
    evap <- rep(5,n)
    inputs[,2] <- evap
    
	  out <- awrar.run(parameters,config,inputs)
	  expect <- inputs[,12]-c(0,diff(vol)/dt)
	  expect_equal(out$outflow,expect,info=sprintf("test %d reservoir",ntest))
	  savetest(parameters,inputs,config,expect)

	})
	
	test_that("river evap - single inflow", {

	  ni <- get.constant("NINPUTSNONROUTING")
	  n <- 20
	  Qin <- seq(100,500,length.out=n)
	  inputs <- cbind(matrix(0,n,ni),Qin)
	  config <- get.default.config()
	  dt <- config[7]
	  config[8] <- 1e3*dt
	  config[9] <- 0.5
	  parameters <- rep(0,10)
    evap <- rep(5,n)
    inputs[,2] <- evap
    
	  out <- awrar.run(parameters,config,inputs)
	  expect <- Qin-evap*1e-3/dt*config[8]*Qin^config[9]
	  expect_equal(out$outflow,expect,info=sprintf("test %d evap",ntest))
	  savetest(parameters,inputs,config,expect)

	})
  
	test_that("three outflows 1", {
	  
	  ni <- get.constant("NINPUTSNONROUTING")
	  n <- 20
	  Qin <- seq(100,500,length.out=n)
	  inputs <- cbind(matrix(0,n,ni),Qin)
	  config <- get.default.config()
    config[6] <- 1 # use anabranch
	  dt <- config[7]
	  config[8] <- 1e3*dt
	  config[9] <- 0.5
	  
    config[12] <- 0.2 # 20% of inflow
    config[14] <- 0.3 # 30% of inflow
	  r1 <-  1-config[12]-config[14]
	  r2 <- config[12]
	  r3 <- config[14]
	  expect <- cbind(Qin*r1,Qin*r2,Qin*r3)
    
    parameters <- rep(0,10)
	  	  
	  out <- awrar.run(parameters,config,inputs)
    delta <- max(abs(cbind(out$outflow,out$states.nonrouting[,20:21])-expect))
    expect_true(delta<1e-10,info=sprintf("test %d three outflows 1",ntest))
	  savetest(parameters,inputs,config,out$outflow)
	  
	})
  
	test_that("three outflows 2", {
	  
	  ni <- get.constant("NINPUTSNONROUTING")
	  n <- 20
	  Qin <- seq(100,500,length.out=n)
	  inputs <- cbind(matrix(0,n,ni),Qin)
	  config <- get.default.config()
	  config[6] <- 1 # use anabranch
	  dt <- config[7]
	  config[8] <- 1e3*dt
	  config[9] <- 0.5
	  
	  config[12] <- 3 # 3/7 of inflow
	  config[14] <- 4 # 4/7 of inflow
	  r1 <- 0
    r2 <- config[12]/(config[12]+config[14])
    r3 <- config[14]/(config[12]+config[14])
	  expect <- cbind(Qin*r1,Qin*r2,Qin*r3)
	  
	  parameters <- rep(0,10)
	  
	  out <- awrar.run(parameters,config,inputs)
	  delta <- max(abs(cbind(out$outflow,out$states.nonrouting[,20:21])-expect))

	  expect_true(delta<1e-10,info=sprintf("test %d three outflows 2",ntest))
	  savetest(parameters,inputs,config,out$outflow)
	  
	})

	test_that("three outflows 3", {
	  
	  ni <- get.constant("NINPUTSNONROUTING")
	  n <- 20
	  Qin <- c(0,0,seq(0,2,length.out=n-2))
	  inputs <- cbind(matrix(0,n,ni),Qin)
	  config <- get.default.config()
	  config[6] <- 1 # use anabranch
	  dt <- config[7]
	  config[8] <- 1e3*dt
	  config[9] <- 0.5
	  
	  config[12] <- 0.6  # q2 <- 0.6 Qin ^ 0.1
	  config[13] <- 0.1
	  config[14] <- 0.2   # q3 <- 2 Qin ^ 0.9
	  config[15] <- 0.9
	  
    q2 <- config[12]*Qin^config[13]
	  q3 <- config[14]*Qin^config[15]
    ratio <- 1*Qin
    ii <- Qin>0
    ratio[ii] <- pmax(1,(q2[ii]+q3[ii])/Qin[ii])
    q2[ii] <- q2[ii]/ratio[ii]
	  q3[ii] <- q3[ii]/ratio[ii]
	  q1 <- Qin-q2-q3
      
    expect <- cbind(q1,q2,q3)
	  
	  parameters <- rep(0,10)
	  
	  out <- awrar.run(parameters,config,inputs)
    res <- cbind(out$outflow,out$states.nonrouting[,20:21])
	  delta <- max(abs(res-expect))
    
	  expect_true(delta<1e-10,info=sprintf("test %d three outflows 3 - comparison",ntest))
    expect_true(min(res)>=0,info=sprintf("test %d three outflows 3 - positive outflow",ntest))
	  savetest(parameters,inputs,config,res)
	  
	})
} else 
{
	cat("\n\n..awrar.run NOT tested ..\n\n")
}
