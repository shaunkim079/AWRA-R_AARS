
# set parameters
riverWaveDuration<-1
river_length<-100
orig_surfaceLayerThickness<-2
orig_riverConductance<-1e-6

# generate data
set.seed(1)
river_depth<-rnorm(5,1,0.5)
river_width<-river_depth*5

# simulation function
sim_fun<-function(par){
  riverConductance<-10^par[1]
  surfaceLayerThickness<-par[2]
  infiltPotentialRiver = riverConductance * river_width * (river_depth / surfaceLayerThickness + 1) * river_length * riverWaveDuration;
  return(infiltPotentialRiver)
}

# generate synthetic observed
obs<-sim_fun(c(log(orig_riverConductance,10),orig_surfaceLayerThickness))

# optimiser function
optimfun<-function(par,surfaceLayerThickness){
  sim<-sim_fun(c(par[1],surfaceLayerThickness))
  out<-sum((obs-sim)^2)
  return(out)
}

optimfun(-6,2)
optimfun(-6,1)

# new surface layer thickness
surfaceLayerThickness<-1

# plot with new surface layer thickness
plot(obs,type="l")
lines(sim_fun(c(-6,surfaceLayerThickness)),col=2,lty=2)

# try to find equivalent riverConductance
library(DEoptim)
opt<-DEoptim(optimfun,surfaceLayerThickness=surfaceLayerThickness,lower=-13,upper=-1,control = list(itermax =10000))
opt$optim$bestval
optimfun(opt$optim$bestmem,surfaceLayerThickness=surfaceLayerThickness)

# plot best fit showing cannot obtain equivalent results
sim_out<-sim_fun(c(opt$optim$bestmem,surfaceLayerThickness))
plot(obs,type="l")
lines(sim_out,col=2,lty=2)




###############################################################################
# optimise using exponents
sim_fun2<-function(par){
  riverConductance<-par[1]
  surfaceLayerThickness<-par[2]
  infiltPotentialRiver = riverConductance * river_width * (river_depth / surfaceLayerThickness + 1) * river_length * riverWaveDuration;
  return(infiltPotentialRiver)
}

optimfun2<-function(par,surfaceLayerThickness){
  sim<-sim_fun2(c(par[1],surfaceLayerThickness))
  out<-sum((obs-sim)^2)
  return(out)
}
# opt<-optim(0,optimfun2,surfaceLayerThickness=surfaceLayerThickness,method = "Brent",upper=0.0001,lower=0)
opt<-DEoptim(optimfun2,surfaceLayerThickness=surfaceLayerThickness,lower=0,upper=0.0001,control = list(itermax =10000))
opt$optim$bestval
sim_out<-sim_fun2(c(opt$optim$bestmem,surfaceLayerThickness))
plot(obs,type="l")
lines(sim_out,col=2,lty=2)

