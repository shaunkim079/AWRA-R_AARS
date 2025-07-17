

trib.routing.run <- function(inflow,lag,K,x,dt=86400){
  
  ierr<-as.integer(0)
  nval<-as.integer(length(inflow))
  dt<-as.double(dt)
  inflow<-as.double(unlist(inflow))
  K<-as.double(unlist(K))
  x<-as.double(unlist(x))
  outflow<-as.double(rep(0,length(inflow)))
  prev_musk_inflow<-musk_rout_vol<-sum_musk_outflow<-musk_instant_outflow<-as.double(0)
  
  NMAXLAG<-10
  uh<-as.double(rep(0,NMAXLAG+4))
  
  trib_routing_run_out<-.C("trib_routing_run",
                           ierr=ierr,
                           nval=nval,
                           dt=dt,
                           inflow=inflow,
                           lag=lag,
                           uh=uh,
                           K=K,
                           x=x,
                           outflow=outflow,
                           prev_musk_inflow=prev_musk_inflow,
                           musk_rout_vol=musk_rout_vol,
                           sum_musk_outflow=sum_musk_outflow,
                           musk_instant_outflow=musk_instant_outflow)
  
  return(trib_routing_run_out$outflow)
}

