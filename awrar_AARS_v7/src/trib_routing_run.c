#include "header.h"


/******* Sub routines**********************************************************/
double max(double v1,double v2);
double minmax(double min,double max,double input);
double monod(double p1,double p2,double input);

void muskingum_runtimestep(int * ierr,
                           double dt,
                           double inflow,
                           double K,
                           double x,
                           double * previous_inflow,
                           double * routing_volume,
                           double * sum_outflow,
                           double * instantaneous_outflow);

void lag_runtimestep(int * ierr,
                     double dt,
                     double inflow,
                     double lag,
                     double * uh
);
void lag_runtimestep_standalone(int * ierr,
                                double * dt, //double dt,
                                double * inflow, //double inflow,
                                double * lag, //double lag,
                                double * uh
);

void trib_routing_run(
  int * ierr,
  int * nval,
  double * dt,
  double * inflow,
  double * lag,
  double * uh,
  double * K,
  double * x,
  double * outflow,
  double * prev_musk_inflow,
  double * musk_rout_vol,
  double * sum_musk_outflow,
  double * musk_instant_outflow
  )
{
  int i;
  double lagged_flow;
  
  // double this_inflow;
  // double this_lag;

  // Loop through time series
  for(i=0;i<*nval;i++)
  {
    // lag subroutine
    // this_inflow = inflow[i];
    // this_lag = lag[i];
    lag_runtimestep(ierr,*dt,
                    inflow[i],lag[i],
                    &(uh[0]));
    // lag_runtimestep_standalone(ierr,dt,
    //                            &(inflow[i]),&(lag[i]),
    //                            &(uh[0]));
    lagged_flow = uh[0];
    // outflow[i] = lagged_flow;
    // Routing subroutine
    muskingum_runtimestep(ierr,
                          *dt,
                          lagged_flow,
                                K[i],
                                x[i],
                                prev_musk_inflow,
                                musk_rout_vol,
                                sum_musk_outflow,
                                musk_instant_outflow);
    outflow[i] = *musk_instant_outflow;
    //printf("prev_musk_inflow=%0.2f --\n",*prev_musk_inflow);
       
    
  }
  return; 
  
}
