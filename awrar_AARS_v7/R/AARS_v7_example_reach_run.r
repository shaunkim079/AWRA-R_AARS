# This file runs an example reach using the AWRA AARS model

# set-up working directory
wd<-"D:/AWRA-R_AARS"
setwd(wd)

# load in libraries
source.awrar.dir<-"awrar_AARS_v7/R"
source.awrar.dir.lf<-list.files(source.awrar.dir,full.names=T)
source.awrar.dir.lf<-source.awrar.dir.lf[grep("test",source.awrar.dir.lf,invert = T)]
for(iii in 1:length(source.awrar.dir.lf)){
  source(source.awrar.dir.lf[iii])
}

if(!is.loaded("awrar_run")){
  if(Sys.info()["sysname"]=="Linux"){
    dyn.load("awrar_AARS_v7/src/awrar_AARS_v7.so")
  } else {
    dyn.load("awrar_AARS_v7/src/awrar_AARS_v7.dll")
  }
}


# standard AARS run ###################
# get input time series file, config file and parameter file
input_file<-"awrar_AARS_v7/data/test_input_416047.csv"
input<-read.csv(input_file,as.is=T)


config_file<-"awrar_AARS_v7/data/test_config_416047.csv"
config<-read.csv(config_file,as.is=T)

parameter_file<-"awrar_AARS_v7/data/test_parameters_416047.csv"
parameters<-read.csv(parameter_file,as.is=T)

# Simulation 
# standard AARS run is when useDynMax=0, for dynamic maximum version set useDynMax=1
# to run with dead storage set useDeadStorage=1 - remember to setup DS size in config
out<-awrar.run(parameters=parameters$x,config=config$x,inputs=input,
               useDynMax=0,useDeadStorage=0)

plot(out$outflow*86.4,type="l",ylim=c(0,max(input$inflow01*86.4)))
lines(input$inflow01*86.4,col=2,lty=2)

head(out$outflow*86.4)

attributes(out)
# out$outflow
# out$states.nonrouting
Salv<-out$states.nonrouting[,colnames(out$states.nonrouting)=="Salv"]/1000
DepthToGW<-out$states.nonrouting[,colnames(out$states.nonrouting)=="DepthToGW"]
riverGroundwaterLoss<-out$states.nonrouting[,colnames(out$states.nonrouting)=="river.groundwater.loss"]

colnames(out$states.nonrouting)




