









setwd("C:/Users/kim079/OneDrive - CSIRO/Documents/bradfield/transmission_losses")

source("packages/awrar_AARS/R/awrar.run.r")
source("packages/awrar_AARS/R/get.constant.r")


if(!is.loaded("awrar_run")){
  if(Sys.info()["sysname"]=="Linux"){
    dyn.load("packages/awrar_AARS/src/awrar_AARS.so")
  } else {
    dyn.load("packages/awrar_AARS/src/awrar_AARS.dll")
  }
}

id<-"416049"
sim_dir<-"//fs1-cbr.nexus.csiro.au/{core}/work/project/WIRADA/AWRA_II/AWRA_LRG/AWRA-LR_v5.00/outputs/jointcalibration/v10/MDB-BorderRivers/simulations"

fn<-paste0("//fs1-cbr.nexus.csiro.au/{core}/work/project/WIRADA/AWRA_II/AWRA_LRG/AWRA-LR_v5.00/inputs/MDB-BorderRivers/combined/combined_",id,"_sitev5.001.csv.gz")
combined<-read.csv(fn,comment="#",as.is = T)
# combined<-read.csv("C:/Users/kim079/OneDrive - CSIRO/Documents/bradfield/transmission_losses/packages/awrar_AARS/data/combined_422010_sitev5.001.csv.gz",as.is=T,comment="#")
combined.run<-combined[,2:27]


fn<-paste0("//fs1-cbr.nexus.csiro.au/{core}/work/project/WIRADA/AWRA_II/AWRA_LRG/AWRA-LR_v5.00/inputs/MDB-BorderRivers/config/config_",id,"_sitev5.001.csv")
config<-read.csv(fn,comment="#",as.is = T)
inflow_ids<-config$config.value[31:40]
inflow_ids<-inflow_ids[!is.na(inflow_ids)]
config.run<-config$config.value[1:18]
# add AARS config items: Xalv, Dalv, SYalv, reach_area
config.run<-c(config.run,200,5,0.1,1e7)


for(i in 1:length(inflow_ids)){
  inflow_file<-paste0(sim_dir,"/",inflow_ids[i],"_full_period_states_nonrouting.csv")
  inflow_states<-read.csv(inflow_file,as.is = T,comment.char = "#")
  inflow_sim<-inflow_states$outflow
  # gap fill
  inflow_final<-combined.run[,16+i]
  inflow_final[inflow_final<0]<-inflow_sim[inflow_final<0]
  
  combined.run[,16+i]<-inflow_final
  
}


fn<-paste0("//fs1-cbr.nexus.csiro.au/{core}/work/project/WIRADA/AWRA_II/AWRA_LRG/AWRA-LR_v5.00/outputs/jointcalibration/v10/MDB-BorderRivers/parameters/parameters_",id,"_v5.001.csv")
parameters<-read.csv(fn,comment="#",as.is = T)
parameters.run<-parameters$value.final
# add AARS parameters items: alpla_alv, beta_alv, gamma_alv
parameters.run<-c(parameters.run[2:13],0.5,0.5,0.5,parameters.run[14:43])

config.run<-as.numeric(config.run)

out<-awrar.run(parameters=parameters.run,config=config.run,inputs=combined.run)

plot(out$outflow,type="l")


head(combined.run)
combined.run$inflow02<-0
combined.run$awral.inflow.top<-0

combined.run$inflow01<-1
# parameters.run[7]<-1
combined.run$rainfall.river<-0
combined.run$evap.river<-0

out<-awrar.run(parameters=parameters.run,config=config.run,inputs=combined.run)
plot(out$outflow,type="l")


Dalv_test<-seq(0.1,100,length.out=10)
# parameters.run[11]<-1
# parameters.run[14]<-1e2
nt<-16252
combined.run<-combined.run[1:nt,]

all_outflow<-c()
all_Salv<-c()
all_riverloss<-c()
for(i in 1:length(Dalv_test)){
  config.run[20]<-Dalv_test[i]
  out<-awrar.run(parameters=parameters.run,config=config.run,inputs=combined.run)
  # plot(out$outflow,type="l")
  # plot(out$states.nonrouting[,34],type="l")
  # plot(out$states.nonrouting[,11],type="l")
  all_outflow<-c(all_outflow,out$outflow[nt])
  all_Salv<-c(all_Salv,out$states.nonrouting[,34][nt])
  all_riverloss<-c(all_riverloss,out$states.nonrouting[,11][nt])
  
}

# SMalv = Xalv * Dalv * SYalv * river_length;
all_SMalv <-  config.run[19] * Dalv_test * config.run[21] * config.run[16];

layout(1:3)
plot(y=all_outflow,x=Dalv_test)
plot(y=all_Salv/all_SMalv,x=Dalv_test)
plot(y=all_riverloss,x=Dalv_test)


stop()
combined.run$inflow01<-0.05
combined.run$inflow01[1:4000]<-0.05
combined.run$inflow01[4001:6000]<-0.01
combined.run$inflow01[6001:9000]<-0.01
combined.run$inflow01[9001:12000]<-0.03

combined.run$inflow01<-combined.run$inflow01*10
parameters.run[12]<-3e-7 # river conductivity

config.run[20]<-5 # dalv
SMalv <-  config.run[19] * config.run[20] * config.run[21] * config.run[16];
out<-awrar.run(parameters=parameters.run,config=config.run,inputs=combined.run)

layout(1:4)
plot(combined.run$inflow01[-(1:600)],type="l",main="inflow",lwd=2)
plot(out$states.nonrouting[,34][-(1:600)]/SMalv,type="l",main="proportion of alluvium saturation",lwd=2)
plot(out$states.nonrouting[,11][-(1:600)],type="l",main="river loss",lwd=2)

# plot(out$outflow[-(1:300)],type="l")

# plot each loss component to see which is taking effect
plot(out$states.nonrouting[,30][-(1:600)]+out$states.nonrouting[,31][-(1:600)],type="l",lwd=2,
     ylim=range(c(out$states.nonrouting[,30][-(1:600)]+out$states.nonrouting[,31][-(1:600)],out$states.nonrouting[,32][-(1:600)]))) # "storeavail + Q"
lines(out$states.nonrouting[,32][-(1:600)],lwd=2,col=2) # "infilt potential"
lines(combined.run$inflow01[-(1:600)],col=3,lwd=2)

# layout(1)
# plot(out$states.nonrouting[,30][-(1:600)]+out$states.nonrouting[,31][-(1:600)],type="l",lwd=2) # "storeavail + Q"
# lines(out$states.nonrouting[,32][-(1:600)],lwd=2,col=2) # "infilt potential"
# lines(combined.run$inflow01[-(1:600)],col=3,lwd=2)


stop()
combined.run$inflow01<-0.05
combined.run$inflow01[1:3000]<-0.2
combined.run$inflow01[3001:6000]<-0.1
combined.run$inflow01[6001:9000]<-0.2
combined.run$inflow01[9001:12000]<-0.1

config.run[20]<-5 # dalv
SMalv <-  config.run[19] * config.run[20] * config.run[21] * config.run[16];
out<-awrar.run(parameters=parameters.run,config=config.run,inputs=combined.run)

layout(1:3)
plot(combined.run$inflow01[-(1:600)],type="l")
plot(out$states.nonrouting[,34][-(1:600)]/SMalv,type="l")
plot(out$states.nonrouting[,11][-(1:600)],type="l")

plot(out$outflow[-(1:300)],type="l")






