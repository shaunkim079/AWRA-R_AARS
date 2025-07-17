#include "header.h"

/******************************************************************************
Lag component of AWRAR.

-- inputs ---
int ierr							Error code
double * inflow,					inflows (m3/s).

-- states ---
double * uh,						previous inflow (m3/s)
double * routing_volume,			volumes (m3)

-- Authors --
Julien Lerat, CSIRO CLW
Justin Hughes, CSIRO CLW

-- Versions --
2012-10-23 - First version of code

*******************************************************************************/
double laguh_ordinate_standalone(double dlag,int index){
	double uho1=0.0,uho0=0.0,val=(double)index;

	if(val-dlag>0) uho0 = val-dlag;
	if(uho0>1) 		uho0=1;

	if(val-dlag+1>0) 	uho1 = val-dlag+1;
	if(uho1>1) 		uho1=1;

	return uho1-uho0;
}

void lag_runtimestep_standalone(int * ierr,
		double * dt, //double dt,
		double * inflow, //double inflow,
		double * lag, //double lag,
		double * uh
	)
{
	int i;
	double dlag = 0;	

	*ierr = 0;

	// Restriction on dlag
	if(*dt>0) dlag = *lag / *dt;
	if(dlag>NMAXLAG) dlag=NMAXLAG;	

	for(i=0;i<NMAXLAG-1;i++) uh[i]=uh[i+1]+laguh_ordinate_standalone(dlag,i) * *inflow;
	uh[NMAXLAG-1] = laguh_ordinate_standalone(dlag,NMAXLAG-1) * *inflow;
	
	//printf("\n\t -- dlag=%0.2f inflow=%0.2f --\n",dlag,*inflow);
	//for(i=0;i<NMAXLAG-1;i++) printf("\tuh[%d]=%0.2f ord=%0.2f\n",i,uh[i],laguh_ordinate_standalone(dlag,i));

	return;
}

