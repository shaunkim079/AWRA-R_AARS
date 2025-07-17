#include "header.h"

/******************************************************************************
Model to compute the mass balance of a reservoir based on volume difference, reservoir area, rainfall and evap

-- inputs ---
double dt,                               time step length (sec)
double inflow,                            inflow before removing the reservoir contribution (m3/s)
double reservoir_volume,                 reservoir volume at the end of time step (m3)
double reservoir_area,                 reservoir area at the end of time step (m3)
double rainfall,             rainfall on reservoir during time step (mm/time step)                        
double evap,                 evap on reservoir during time step (mm/time step)

-- states ---
double * previous_volume,      reservoir volume at beginning of time step (m3)
double * previous_volume,      reservoir area at beginning of time step (m3)
double * rainfall_flux,        rainfall flux on reservoir during time step (m3/s)
double * evap_flux,            evap flux from reservoir during time step (m3/s)
double * reservoir_contribution     reservoir contribution over time step (m3/s)
double * carryover_reservoir_flux   flux that can attributed during the current time step (m3/s)

-- Authors --
Julien Lerat, CSIRO CLW

-- Versions --
2012-11-07 - First version of the code

*******************************************************************************/

void reservoir_runtimestep(int * ierr,
		double dt,
		double inflow,
		double reservoir_volume,
		double reservoir_area,
		double rainfall,
		double evap,
		double * previous_volume,
		double * previous_area,		
		double * rainfall_flux,
		double * evap_flux,
		double * reservoir_contribution,
		double * carryover_reservoir_flux,
		double reservoir_net_diversion
	)
{
    double v1,v2,a1,a2,dv,contribution;       
	
    // Get data
    v1 = *previous_volume;
    v2 = reservoir_volume;
     
    a1 = *previous_area;
    a2 = reservoir_area;

    *rainfall_flux=0;
    *evap_flux = 0;
    
    // Initialise
    dv = 0;
    contribution = 0;            

 		// Check data (1m3 or 1m2 is the mimimum acceptable vol/area respectively)
		if((v1>= 1) & (v2>= 1))
		{
			// Computes change in volume terms (-ve contribution means water is captured by the storage)
			dv = (v1-v2)/dt; // m3/s

			// Computes climate terms 
			*rainfall_flux = rainfall*1e-3/dt*(a1+a2)/2;  // m3/s
			*evap_flux = evap*1e-3/dt*(a1+a2)/2;  // m3/s

			//contribution = dv + *rainfall_flux - *evap_flux + *carryover_reservoir_flux;
			contribution = dv + *rainfall_flux - *evap_flux;
			
			// Account for reservoir diversions (+ve net diversion is water taken away)
			contribution -= reservoir_net_diversion;

		}

	
		// Check mass balance
		*carryover_reservoir_flux = 0;
		if(inflow+contribution<0)
		{
		  *carryover_reservoir_flux=contribution+inflow; // Keep track of unattributed fluxes
       	  contribution= -inflow;
    	}
		
		// Loop
		*reservoir_contribution = contribution;
		*previous_volume = v2;
		*previous_area = a2;

    return;				
}

