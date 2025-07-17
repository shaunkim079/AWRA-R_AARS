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
# add AARS config items: Xalv, Dalv, reach_area # removed SYalv
config.run<-c(config.run,200,5,1e7)


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

# previous results
fn<-paste0("//fs1-cbr.nexus.csiro.au/{core}/work/project/WIRADA/AWRA_II/AWRA_LRG/AWRA-LR_v5.00/outputs/jointcalibration/v10/MDB-BorderRivers/simulations/",id,"_full_period_states_nonrouting.csv")
prev_out<-read.csv(fn,as.is=T,comment.char = "#")



obs_outflow<-combined$outflow01
obs_outflow[obs_outflow<0]<-NA

indices_to_run<-5000:10000
obs_outflow<-obs_outflow[indices_to_run]
combined.run<-combined.run[indices_to_run,]
prev_out<-prev_out[indices_to_run,]
dates<-combined[indices_to_run,1]
dates

library(hydroGOF)

ee<-min(obs_outflow[obs_outflow>0],na.rm = T)
NSE(obs=log(obs_outflow+ee),sim=log(prev_out$outflow+ee))

plot(obs_outflow,type="l",log="y")
lines(prev_out$outflow,col=2,lty=2)


optim_sim<-function(pars){
  config_new<-config.run
  # config_new[19:21]<-pars[1:3]
  config_new[19]<-pars[1]*100
  config_new[20]<-5
  # config_new[21]<-pars[2]/10
  config_new[21]<-10^pars[5]
  par_new<-parameters.run
  par_new[12]<-10^pars[3]
  # par_new[14:16]<-pars[4:6]
  par_new[8]<-pars[2]/10
  par_new[9]<-10^pars[4]
  out<-awrar.run(parameters=par_new,config=config_new,inputs=combined.run)
  return(out)
}
optim_fun<-function(pars){
  out<-optim_sim(pars)
  log_nse<-NSE(obs=log(obs_outflow+ee),sim=log(out$outflow+ee))
  return(-log_nse)
}
# opt<-optim(c(2,1,-6,-6),optim_fun)
# 
# opt
# sim_opt<-optim_sim(opt$par)
# plot(obs_outflow,type="l",xlim=c(5000,10000),log="y")
# lines(sim_opt$outflow,col=2,lty=2)

library(DEoptim)
opt<-DEoptim(optim_fun,lower=c(0.01,0.0001,-10,-10,0),upper=c(10,10,10,10,10))

sim_opt<-optim_sim(opt$optim$bestmem)

plot(obs_outflow,type="l",log="y",xlim=c(4000,4500))
lines(sim_opt$outflow,col=2,lty=2)
lines(prev_out$outflow,col=3,lty=3)

plot(sim_opt$states.nonrouting[,30]+sim_opt$states.nonrouting[,31],type="l",lwd=2,
     ylim=range(c(sim_opt$states.nonrouting[,30]+sim_opt$states.nonrouting[,31],sim_opt$states.nonrouting[,32]))) # "storeavail + Q"
lines(sim_opt$states.nonrouting[,32],lwd=2,col=2) # "infilt potential"
lines(combined.run$inflow01,col=3,lwd=2)

plot(sim_opt$states.nonrouting[,30],type="l",lwd=2,
     ylim=range(c(sim_opt$states.nonrouting[-c(1:100),30]+sim_opt$states.nonrouting[-c(1:100),31]))) # "storeavail"
lines(sim_opt$states.nonrouting[,31],lwd=2,col=2) # "Q"
lines(sim_opt$states.nonrouting[,32],lwd=2,col=3) # "infilt potential"
lines(combined.run$inflow01,col=4,lwd=2)


# combined.run$rainfall.river<-0
# combined.run$evap.river<-0
# sim_opt<-optim_sim(opt$optim$bestmem)


layout(1:2)
plot(sim_opt$states.nonrouting[,30],type="l",lwd=2,
     ylim=range(c(sim_opt$states.nonrouting[-c(1:100),30]+sim_opt$states.nonrouting[-c(1:100),31])),xlim=c(4020,4050)) # "storeavail"
lines(sim_opt$states.nonrouting[,31],lwd=2,col=2) # "Q"
lines(sim_opt$states.nonrouting[,30]+sim_opt$states.nonrouting[,31],lwd=2,col=6) # "storeavail+Q"
lines(sim_opt$states.nonrouting[,32],lwd=2,col=3) # "infilt potential"
lines(combined.run$inflow01,col=4,lwd=2)
lines(combined.run$awral.inflow.top,col=5,lwd=2)
lines(sim_opt$states.nonrouting[,36]/86400,lwd=2,col=7) # "alluvium rain vol"
lines(sim_opt$states.nonrouting[,37]/86400,lwd=2,col=8) # "alluvium evap vol"

plot(combined.run$evap.river,lwd=2,type="l",xlim=c(4020,4050),ylim=c(0,10))
lines(combined.run$rainfall.river,lwd=2,col=2,lty=2)


plot(sim_opt$states.nonrouting[,11],type="l",main="river loss",lwd=2,xlim=c(4000,4500),ylim=c(0,0.2))
lines(sim_opt$states.nonrouting[,32],lwd=2,col=2,lty=2) # "infilt potential"


NSE(obs=log(obs_outflow+ee),sim=log(sim_opt$outflow+ee))
NSE(obs=log(obs_outflow[1:8000]+ee),sim=log(sim_opt$outflow[1:8000]+ee))
NSE(obs=log(obs_outflow[8001:length(obs_outflow)]+ee),sim=log(sim_opt$outflow[8001:length(obs_outflow)]+ee))


png("C:/Users/kim079/OneDrive - CSIRO/Documents/WERP/documents/new_model_plot.png",res=300,width=2000,height=2500)
layout(1:3)
plot(obs_outflow+1e-4,log="y",xlim=c(4000,4500),lwd=2,col=1,lty=1,type="l",main=paste0("Inflows and outflows for reach ",id),
     axes = F,xlab="Date",ylab="Discharge (cumecs)")
box()
axis(2)
indices<-grep("-01$",dates)
axis(1,at=indices,labels=dates[indices])
lines(combined.run$inflow01+1e-4,col=2,lwd=2,lty=2)
legend("topleft",lty=c(1,2),lwd=c(2,2),col=c(1,2),legend = c("Observed outflow","Observed inflow"))


plot(obs_outflow+1e-4,type="l",log="y",xlim=c(4000,4500),lwd=2,col=1,lty=1,
     main=paste0("Observed and simulated outflows for reach ",id),
     axes = F,xlab="Date",ylab="Discharge (cumecs)")
box()
axis(2)
indices<-grep("-01$",dates)
axis(1,at=indices,labels=dates[indices])
lines(sim_opt$outflow+1e-4,col=3,lty=3,lwd=3)
lines(prev_out$outflow+1e-4,col=4,lty=4,lwd=2)
legend("topleft",lty=c(1,3,4),lwd=c(2,3,2),col=c(1,3,4),legend = c("Observed outflow","New AWRA-R","Old AWRA-R"))


plot(sim_opt$states.nonrouting[,11],type="l",main=paste0("River loss for reach ",id),lwd=2,xlim=c(4000,4500),ylim=c(0,0.2),
     axes = F,xlab="Date",ylab="Loss (cumecs)")
box()
axis(2)
indices<-grep("-01$",dates)
axis(1,at=indices,labels=dates[indices])

# lines(sim_opt$states.nonrouting[,32],lwd=2,col=2,lty=2) # "infilt potential"
dev.off()

stop()


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






