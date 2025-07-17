#include "header.h"

/******************************************************************************
Muskingum routing component of AWRAR.
The model runs multiple inflows (ninflow) toward a downstream point.

dV/dt = I - O
V = k [xI + (1-x) O]
<=>
k [ x dI/dt + (1-x) dO/dt ] = I - O
<=>
dO/dt = I/(1-x)/k - x/(1-x) dI/dt - O/(1-x)/k

if I(t) = I0 (constant inflow), we get
dO/dt = I/(1-x)/k - O/(1-x)/k

if I(t) = (I1-I0)/d*t+I0 (linear inflow), we get
dO/dt = [ (I1-I0)/d*t+I0 ] /(1-x)/k - x/(1-x) (I1-I0)/d - O/(1-x)/k

-- inputs ---
int ierr							Error code
double dt,							time step length (sec)
double inflow,						inflows (m3/s).
double K							First muskingum parameter
double x							Second muskingum parameter

-- states ---
double * previous_inflow,			previous inflow (m3/s)
double * routing_volume,			volumes (m3)
double *outflow,					outflows (m3/s)

-- Authors --
Julien Lerat, CSIRO CLW
Justin Hughes, CSIRO CLW

-- Versions --
2012-10-23 - First translation of Justin's fortran code

-- Original fortran code --

c sample subroutine to include for R
       SUBROUTINE rout(nrowdat,ntrib,outRout, musky, lagdat) 
       implicit none
       INTEGER nrowdat, ntrib, i, n
       DOUBLE PRECISION outRout(nrowdat, ntrib+1), musky(ntrib+1,3), 
     1 lagdat(nrowdat, ntrib+1)

       outRout=0.D0
       DO n=1,ntrib+1
       outRout(1,n)=lagdat(1,n)
       ENDDO

       DO i=2,nrowdat
          DO n=1,ntrib+1
          outRout(i, n)=musky(n,1)*lagdat(i,n)+musky(n,2)*
     1    lagdat(i-1,n)+musky(n,3)*outRout(i-1,n)
          ENDDO
       ENDDO
       END SUBROUTINE    

*******************************************************************************/

// Bounds on K and x to get positive outflows
double actual_x_standalone(double dt,double x,double K)
{
	double actualx;

	// Bounds
	if(x<0) x=0;
	if(x>1) x=1;
	if(K<0) K=0;

	actualx = x;
	if((actualx*K>=dt/2) & (K>0))		actualx = dt/2/K;
	if((K*(1-actualx)<=dt/2) & (K>0))  	actualx = 1-dt/2/K;
	if(K==0) 						actualx = 0;

	if(actualx<0) actualx=0;
	if(actualx>1) actualx=1;

	return actualx;
}

// R wrapper function
void get_actual_x_standalone(int * ierr,
	double * dt,double * x,double * K, double * actualx)
{
	*actualx = 	actual_x_standalone(*dt,*x,*K);
	return;
}


void muskingum_runtimestep_standalone(int * ierr,
		double * dt,
		double * inflow,
		double * K,
		double * x,
		double * previous_inflow,
		double * routing_volume,
		double * sum_outflow,
		double * instantaneous_outflow
	)
{
		double c0,c1,c2,actualx,D; //,instantaneous_outflow_prec;
		
		*ierr = 0;
    //instantaneous_outflow_prec =   *instantaneous_outflow;
    
		// Restriction in K
		if(*K<=0)
			*instantaneous_outflow = *inflow;
		else
		{
		    if(*K < *dt/2) *K = *dt/2;

			// Restriction in x
			if(*x<0) *x=0;
			if(*x>1) *x=1;
			actualx=actual_x_standalone(*dt,*x,*K);

			// Muskingum constants obtained by integrating the instantaneous storage
			// equation over the boundaries of the time step and assuming linear
			// variations of the inflow and outflow within the time step
			// This integration method is the modified Euler (or mid-point, or )
			D = *K * (1-actualx) + *dt/2;
			c0 = -(*K * actualx - *dt/2)/D;
			c1 = (*K * actualx + *dt/2)/D;
			c2 = (*K - *K * actualx - *dt/2)/D;

			*instantaneous_outflow = c0 * *inflow 
					+ c1 * *previous_inflow 
					+ c2 * *instantaneous_outflow;
		}

		*sum_outflow +=	*instantaneous_outflow * *dt;
		// This is wrong. It should be 
		// (*instantaneous_outflow + instantaneous_outflow_prec)/2* *dt
		//*routing_volume += (*inflow - *instantaneous_outflow) * *dt;
		double routing_volume_tmp = *routing_volume;
		routing_volume_tmp 	+= 	(*inflow-*instantaneous_outflow) * *dt;
		if(routing_volume_tmp<0.0){
		  *instantaneous_outflow = (*routing_volume / *dt)+*inflow;
		  *routing_volume = 0.0;
		} else {
		  *routing_volume = routing_volume_tmp;
		}
		
    *previous_inflow = *inflow;

}

