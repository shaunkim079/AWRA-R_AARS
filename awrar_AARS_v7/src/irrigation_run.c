#include "header.h"

/******************************************************************************
Code to loop through timeseries and implement the timestep code irrigation_runtimestep.c

-- inputs ---
int * ierr,			Error code

int *nday, 			Dimensions of array (nday = number of timesteps)
int *nconfig,
int * ninputs,
int * npar,
int * nstates,

double * config, 		configuration data (see awrar_runtimestep.c)
double * inputs_array,	inputs data (see awrar_runtimestep.c)
double * parameters,	model parameters (see awrar_runtimestep.c)
double * states_array,	model states (see awrar_runtimestep.c)

-- states ---
double * states_array 	model states (see awrar_runtimestep.c)

-- Authors --
Julien Lerat and Ang Yang, CSIRO CLW

-- Versions --
2013-08-22 - Second version of the code

*******************************************************************************/
double minmax(double min,double max,double input);

void irrigation_runtimestep(
		int * ierr,
		double timestep_length,				//time step length in second
		double soilCap,						//mm
		double gamma,						//m2
		double sigma,						//m
		double areaIrrig_max,				//m2
		double licVol,						//m3
		double volOFSmax,					//m3
		double volGWmax,					//m3
		double efficiency,
		double returnflow_coef,
		double ringtankAvgDepth,			//m
		double syAq,                         // dimensionless    ***NEW***
        double kSurf,                        // (m/s)            ***NEW***
		double reachInflow,					// (m3/s)
		double allocation,
		double weighted_Kc,
		double areaActivePro,
		double julianDay,
		double rainfall,					//(mm/timestep)
		double evap,						//(mm/timestep)
        double depthToGW,                    // (m)              ***NEW***
		double alpha_irrig,
		double beta_irrig,
		double threshOFS,					//(m3/s)
		double maxPump,						//(m3/s)
		double pumpAdjust,					//(m3/s)
		double * licPro,
		double * OFSpro,
		double * GWpro,
		double * areaActivePro_prec,
		double * areaCurrent,				//m2
		double * demand,					//(m3/s)
		double * volOFS,					//m3
		double * volOFSin,					//(m3/s)
		double * areaOFS,					//m2
		double * soil,						//m
		double * runoff,					//(m3/s)
		double * gation,					//(m3/s)
		double * diversion,					//(m3/s)
        double * flowLimitDiv,               //(m3/s) internal - for calculation of diversion limited by supply from the river system
		double * drainage,					//(m3/s)
		double * volGWout,					//(m3/s)
		double * volOFSout,					//(m3/s)
		double * GWleft,					//m3
		double * licUsed,					//(m3)
		double * licWorking,					//(m3)
        double * rechargeGWir,               // (m3/s) state for export as time series in STATES object ***NEW***
        double * deltaS,                       //internal state  (m/timestep)  ***NEW***
        double * Infiltration                 // internal state (m/timestep) ***NEW***
		);

void irrigation_run(
		int * ierr,
		int *nday, int *nconfig, int * ninputs,int * npar,
	    int * nstates,
		double * config,
		double * inputs_array,
		double * parameters,
		double * states_array)
{
  	int i,k,debugflag,imax_debug=3;
	double states[NSTATESIRRIG];

	debugflag=1;

	if(debugflag>0)
		printf("\n\t\tnday=%d\n\t\tnconfig=%d\n\t\tninputs=%d\n\t\tnpar=%d\n\t\tnstates=%d\n",
			*nday,*nconfig,*ninputs,*npar,*nstates);

	// Initialise states
	for(i=0;i<NSTATESIRRIG;i++) states[i] = states_array[0];

	// Loop through time series
  	for(i=0;i<*nday;i++)
  	{
		if((debugflag>1) & (i < imax_debug))
		{
			printf("\n\t\tirrigation_run : time step %d\n",i);
			printf("\n\t\t-> config = ");
			for(k=0;k<*nconfig;k++) printf("%0.2f ",config[k]);
			printf("\n\t\t-> parameters = ");
			for(k=0;k<*npar;k++) printf("%0.2f ",parameters[k]);
			printf("\n\t\t-> inputs = ");
			for(k=0;k<*ninputs;k++) printf("%0.2f ",inputs_array[i* *ninputs+k]);

		}
/*
  		irrigation_runtimestep(
				ierr,
				nconfig,ninputs,npar,nstates,
				config,
				&(inputs_array[i* *ninputs]),
				parameters,
				states);
*/
		double timestep, areaIrrig_max, licVol, volOFSmax, volGWmax, efficiency, returnflow_coef, soilCap, gamma, sigma, ringtankAvgDepth,syAq,kSurf,
		alpha_irrig, beta_irrig, threshOFS, maxPump, pumpAdjust,
		rainfall, evap, allocation, weighted_Kc, areaActivePro, julianDay, reachInflow, depthToGW;

		//config
		timestep        = config[0];
		//timestep = minmax(1,LARGEVALUE,timestep);

		areaIrrig_max 		= config[1]; // Max irrigated area (km2)
		//areaIrrig_max = minmax(0,1e11,areaIrrig_max); // Max = 1e11 m2 = 10 000 Ha

		licVol 				= config[2];
		//licVol = minmax(0,1e6,licVol); // Max = 1e6 m3 = 1000 GL

		volOFSmax 			= config[3];
		//volOFSmax = minmax(0,1e6,volOFSmax); // Max = 1e6 m3 = 1000 GL

		volGWmax 				= config[4];
		//volGWmax = minmax(0,1e6,volGWmax); // Max = 1e6 m3 = 1000 GL

		efficiency 			= config[5];
		//efficiency = minmax(0,10,efficiency); // I don't understand why efficieny >1 ?

		returnflow_coef = config[6];
		returnflow_coef = minmax(0,1,returnflow_coef);

		soilCap = config[7];
		gamma = config[8];
		sigma = config[9];
		ringtankAvgDepth = config[10];

		// new config items here (November 2013)                      ****NEW****
		syAq   = config[11]; // new config item specific yield aquifer  (dimensionless, range=0 - 0.5) ****NEW****
		kSurf = config[12]; // new config item, surface layer conductivity (m/s) ****NEW****

		//parameters
		alpha_irrig = parameters[0];
		//alpha_irrig = minmax(0,1e30,alpha_irrig);

		beta_irrig = parameters[1];
		//beta_irrig = minmax(0,1e30,beta_irrig);

		threshOFS = parameters[2];
		//threshOFS = minmax(0,1e30,threshOFS);

		maxPump = parameters[3];
		//maxPump = minmax(0,1e30,maxPump);

		pumpAdjust = parameters[4];
		//pumpAdjust = minmax(0,1e30,pumpAdjust);

		//inputs
		rainfall        = inputs_array[i* *ninputs+0];
		//rainfall = minmax(0,5e2,rainfall);

		evap            = inputs_array[i* *ninputs+1];
		//evap = minmax(0,5e1,evap);

		allocation  = inputs_array[i* *ninputs+2];
		allocation = minmax(0,1,allocation); // Allocation within 0 to 100%

		weighted_Kc = inputs_array[i* *ninputs+3];
		//weighted_Kc = minmax(0,5,weighted_Kc); // Max KC set to 5. Could be lower, probably 2

		areaActivePro = inputs_array[i* *ninputs+4];
		areaActivePro = minmax(0,1,areaActivePro); // Proportion

		julianDay = inputs_array[i* *ninputs+5];
		julianDay = minmax(1,366,julianDay);

		reachInflow = inputs_array[i* *ninputs+6];
		
		// new input array item here (November 2013)
		depthToGW = inputs_array[i* *ninputs+7]; // depth to groundwater time series (m)     ***NEW ***
		
		/* commented out by Ang, agreed by Justin on 26/02/2014
		if(i==0)
		{
			states[4] = areaIrrig_max / 10;
			states[5] = (weighted_Kc * evap - rainfall * areaActivePro) * states[4] / 1000 / timestep;
			states[6] = volOFSmax;
			states[8] = volOFSmax / ringtankAvgDepth;
			states[9] = 100.0 / 1000;
			states[19] = allocation * licVol;
			states[17] = volGWmax;
			states[0] = (states[19] - states[18])/(states[19]-states[18] + states[6] + volGWmax);
			states[1] = states[6] / (states[19] - states[18] + states[6] + volGWmax);
			states[2] = volGWmax / (states[19] - states[18] + states[6] + volGWmax);
			states[3] = areaActivePro;
		}
		else */
		{
			// Irrigation model
			irrigation_runtimestep(
				ierr, timestep,
				soilCap,gamma,sigma,
				areaIrrig_max,licVol,volOFSmax,volGWmax,efficiency,returnflow_coef,ringtankAvgDepth,syAq,kSurf,
				reachInflow,allocation,weighted_Kc,areaActivePro,julianDay,rainfall,evap,depthToGW,
				alpha_irrig,beta_irrig,threshOFS,maxPump,pumpAdjust,
				&(states[0]),&(states[1]),&(states[2]),
				&(states[3]),&(states[4]),&(states[5]),
				&(states[6]),&(states[7]),&(states[8]),
				&(states[9]),&(states[10]),&(states[11]),
				&(states[12]),&(states[13]),&(states[14]),
				&(states[15]),&(states[16]),&(states[17]),
				&(states[18]),&(states[19]),
				&(states[20]),&(states[21]),&(states[22]));
		}

		// store states
		for(k=0;k<*nstates;k++) states_array[i* *nstates+k] = states[k];

		if((debugflag>1) & (i < imax_debug))
		{
			printf("\n\t\t-> states = ");
			for(k=0;k<NSTATESIRRIG;k++)
					printf("%0.2f (%d) ",states[k],k);

			printf("\n\n");
		}

  		if(*ierr>0) return;
	}
}
