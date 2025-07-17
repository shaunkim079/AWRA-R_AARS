#include "header.h"

/******************************************************************************
Code to loop through timeseries and implement the timestep code awrar_runtimestep.c

-- inputs ---
int * ierr,			Error code

int *nval, 			Dimensions of array (nval = number of timesteps)
int *nconfig, 
int * ninputs,
int * npar,
int * nstates, 

double * config, 		configuration data (see awrarirrig_runtimestep.c)
double * inputs_array,	inputs data (see awrarirrig_runtimestep.c)
double * parameters,	model parameters (see awrarirrig_runtimestep.c)
double * states_array,	model states (see awrarirrig_runtimestep.c)

double * use_irrig,			switch for irrigation model (see awrarirrig_runtimestep.c)
double * irrigConfig, 		irrigation configuration data (see awrarirrig_runtimestep.c)
double * irrigInputs_array,	irrigation inputs data (see awrarirrig_runtimestep.c)
double * irrigParameters,	irrigation model parameters (see awrarirrig_runtimestep.c)
double * irrigStates_array,	irrigation model states (see awrarirrig_runtimestep.c)

double * is_headwater,	estimates river states but outflow is rainfall-runoff multiplied by scaling factor

-- states ---
double * states_array 	model states (see awrarirrig_runtimestep.c)
double * irrigStates_array 	irrigation model states (see awrarirrig_runtimestep.c)

-- Authors --
Julien Lerat, CSIRO CLW
Ang Yang, CSIRO CLW

-- Versions --
2012-10-23 - First version of the code
2014-02-26 - update to include irrigation module

*******************************************************************************/

void awrar_runtimestep(
		int * ierr,
		int * nconfig,
		int * ninputs,
		int * npar,
    	int * nstates,
		double * config,
		double * inputs,
		double * parameters,
		double * states,
		double * use_irrig,
		double * irrigConfig,
		double * irrigInputs,
		double * irrigParameters,
		double * irrigStates,
		double * is_headwater,
		int timestep_index,
		double * useDynMax,
		double * useDeadStorage);
    				
void awrar_run(
		int * ierr,
		int *nval, int *nconfig, int * ninputs,int * npar,
	    int * nstates,
		double * config,
		double * inputs_array,
		double * parameters,
		double * states_array,
		double * use_irrig,
		double * irrigConfig,
		double * irrigInputs_array,
		double * irrigParameters,
		double * irrigStates_array,
		double * is_headwater,
		double * useDynMax,
		double * useDeadStorage)
{
  	int i,k,debugflag,imax_debug;
	double states[NSTATESNONROUTING+NINFLOWMAX*NSTATESROUTING];
	double irrigStates[NSTATESIRRIG];

	debugflag=0;

	if(debugflag>0) 
		printf("\n\t\tnval=%d\n\t\tnconfig=%d\n\t\tninputs=%d\n\t\tnpar=%d\n\t\tnstates=%d\n",
			*nval,*nconfig,*ninputs,*npar,*nstates);

	// Initialise states
	for(i=0;i<NSTATESNONROUTING+NINFLOWMAX*NSTATESROUTING;i++) states[i] = states_array[i];
	if(* use_irrig == 1){
		for(i=0;i<NSTATESIRRIG;i++) irrigStates[i]=irrigStates_array[i];
	}
	
	// Loop through time series
  	for(i=0;i<*nval;i++)
  	{
		if((debugflag>1) & (i < imax_debug))
		{ 
			printf("\n\t\tawrar_run : time step %d\n",i);
			printf("\n\t\t-> config = ");
			for(k=0;k<*nconfig;k++) printf("%0.2f ",config[k]);
			printf("\n\t\t-> parameters = ");
			for(k=0;k<*npar;k++) printf("%0.2f ",parameters[k]);
			printf("\n\t\t-> inputs = ");
			for(k=0;k<*ninputs;k++) printf("%0.2f ",inputs_array[i* *ninputs+k]);

			printf("\n\t\t-> irrigConfig = ");
			for(k=0;k<NCONFIGIRRIG;k++) printf("%0.2f ",irrigConfig[k]);
			printf("\n\t\t-> irrigParameters = ");
			for(k=0;k<NPARIRRIG;k++) printf("%0.2f ",irrigParameters[k]);
			printf("\n\t\t-> irrigInputs = ");
			for(k=0;k<NINPUTSIRRIG;k++) printf("%0.2f ",irrigInputs_array[i* NINPUTSIRRIG+k]);
		}

	if(* use_irrig == 1){
		awrar_runtimestep(
			ierr,
			nconfig,
			ninputs,
			npar,
			nstates,
			config,
			&(inputs_array[i* *ninputs]),
			parameters,
			states,
			use_irrig,
			irrigConfig,
			&(irrigInputs_array[i* NINPUTSIRRIG]),
			irrigParameters,
			irrigStates,
			is_headwater,
			i,
			useDynMax,
			useDeadStorage);
	}else{
		awrar_runtimestep(
			ierr,
			nconfig,
			ninputs,
			npar,
			nstates,
			config,
			&(inputs_array[i* *ninputs]),
			parameters,
			states,
			use_irrig,
			NULL,
			NULL,
			NULL,
			NULL,
			is_headwater,
			i,
			useDynMax,
			useDeadStorage);
	}
	
		// store states
		for(k=0;k<*nstates;k++) states_array[i* *nstates+k] = states[k];	
		if(* use_irrig == 1)
			for(k=0;k<NSTATESIRRIG;k++) irrigStates_array[i* NSTATESIRRIG+k] = irrigStates[k];	
			
		if((debugflag>1) & (i < imax_debug))
		{ 
			printf("\n\t\t-> states (non routing) = ");
			for(k=0;k<NSTATESNONROUTING;k++) 
					printf("%0.2f (%d) ",states[k],k);
			
			if(* use_irrig == 1){
				printf("\n\t\t-> states (irrig) = ");
				for(k=0;k<NSTATESIRRIG;k++) 
						printf("%0.2f (%d) ",irrigStates[k],k);
			}

			if(debugflag>2)
			{
				printf("\n\t\t-> states (routing-nonlag) = ");
				for(k=NSTATESNONROUTING;k<4+NSTATESNONROUTING;k++) 
						printf("%0.2f (%d) ",states[k],k);

				printf("\n\t\t-> states (routing-lag) = ");
				for(k=NSTATESNONROUTING+4;k<NSTATESROUTING+NSTATESNONROUTING;k++) 
						printf("%0.2f (%d) ",states[k],k);
			}
			printf("\n\n");
		}

  		if(*ierr>0) return;    
  
	}

}
