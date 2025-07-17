context("get.actual.x")

test_that("test x range", {

	dt <- 86400
	pp <- expand.grid(K=seq(0,dt*10,length.out=20),x=seq(0,1,length.out=20))
	pp <- data.frame(pp,ax=NA)
	for(i in 1:nrow(pp))
		pp$ax[i] <- get.actual.x(pp$x[i],pp$K[i],dt)

#	matplot(pp$K,cbind(pp$x,pp$ax))
#	browser()
})
