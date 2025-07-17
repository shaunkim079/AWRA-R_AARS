context("subperiods")

test_that("subperiods - basic test", {
	nval <- 1000
	nper <- 3
	warmup <- 100
	validata <- sort(sample(1:nval,nval/2))
	s<-subperiods(nval,nper,warmup,validata)
	expect_that(as.double(s$startend[,1]),equals(c(1,301,600)))
	expect_that(as.double(s$startend[,2]),equals(c(401,700,1000)))
})

test_that("validity of subperiods", {
	nval <- 1000
	nper <- 3
	warmup <- 100
	validata <- sort(sample(1:nval,nval/2))
	sb<-subperiods(nval,nper,warmup,validata)
	cs <- checksubperiods(sb)
	expect_true(cs$passed,info=cs$message)
})


test_that("subperiods with nper = 1", {
	nval <- 1000
	nper <- 1
	warmup <- 100
	validata <- sort(sample(1:nval,nval/2))
	sb<-subperiods(nval,nper,warmup,validata)
	cs <- checksubperiods(sb)
	expect_true(cs$passed,info=cs$message)
})

