//#include "stdafx.h"
#include "header.h"

/******************************************************************************
Timestep code of AWRA-R running the routing and floodplain component

-- inputs ---
int * ierr,				Error code

int * nconfig, 			dimensions of arrays
int * ninputs,
int * npar,
int * nstates,

double * config       Configuration data
    config[0] : use routing (=1) or not (=0)
    config[1] : use flood model (=1) or not (=0)
    config[2] : use monod model (=1) or not (=0)
    config[3] : use reservoir model (=1) or not (=0)
    config[4] : use ungauged inflows (=1) or not (=0)
    config[5] : use anabranch (=1) or not (=0)
    config[6] : time step duration (seconds)
    config[7] : river area/volume relationship, \eqn{\alpha}
    config[8] : river area/volume relationship, \eqn{\beta}
    config[9] : floodplain area/volume relationship, \eqn{\alpha}  (m^-1)
    config[10] : floodplain area/volume relationship, \eqn{\beta}  (m2)
    config[11] : anabranch loss - upstream of floodplain, parameter a
    config[12] : anabranch loss - upstream of floodplain, parameter b
    config[13] : anabranch loss - downstream of floodplain, parameter a
    config[14] : anabranch loss - downstream of floodplain, parameter b
    config[15] : floodplain length (m)
	config[16] : river depth/flow relationship, \eqn{\alpha}
	config[17] : river depth/flow relationship, \eqn{\beta}
	
double * inputs,      input data. 1D Array ninputs x 1
    inputs[0] : rainfall river (mm/d)
    inputs[1] : evap river         (mm/d)
    inputs[2] : rainfall floodplain     (mm/d)
    inputs[3] : evap floodplain        (mm/d)
    inputs[4] : irrigation diversion   (m3/s)
    inputs[5] : irrigation return flow  (m3/s)
    inputs[6] : diversion for urban water supply  (m3/s)
    inputs[7] : ungauged inflow - top 1  (m3/s)
    inputs[8] : ungauged inflow - top 2  (m3/s)
    inputs[9] : reservoir volume  (m3)
    inputs[10] : reservoir area    (m2)
	inputs[11] : depth to groundwater    (m)
	inputs[12] : river depth    (m)
	inputs[13] : river width    (m)
	...
    inputs[NINPUTSNONROUTING] : 	inflow 1      (m3/s)
    inputs[NINPUTSNONROUTING+1] : 	inflow 2    (m3/s)
    inputs[NINPUTSNONROUTING+NINFLOWMAX] : ... (other inflows) (m3/s)

double * parameters
	parameters[0] =	return_flow_coefficient   (-)
	parameters[1] =	floodplain Ksat    (m/s)
	parameters[2] =	monod1   (-)
	parameters[3] = monod2   (m3/s)
	parameters[4] = overbank flow threshold   (m3/s)
	parameters[5] = flood Gamma (overbank flow function power exponent)
	parameters[6] = runoff correction factor (-)
	parameters[7] = aquifer specific yield (-)
	parameters[8] = aquifer Ksat (m/s)
	parameters[9] = aquifer thickness (m)
	parameters[10] = surface layer thickness (m)
	parameters[11] = river conductance (m/s)
	...
	(parameters routing inflow 1)
    parameters[NPARNONROUTING] = lag inflow 1    (sec)
    parameters[NPARNONROUTING+1] = K inflow 1    (sec)
    parameters[NPARNONROUTING+2] = x inflow 1     (-)
	...
	(parameters routing inflow 2)
    parameters[NPARNONROUTING+NPARROUTING] = lag inflow 2   (sec)
    parameters[NPARNONROUTING+NPARROUTING+1] = K inflow 2   (sec)
    parameters[NPARNONROUTING+NPARROUTING+2] = x inflow 2   (-)
  ...
	(parameters routing inflow 3)
    parameters[NPARNONROUTING+2*NPARROUTING] = lag inflow 3   (sec)
    parameters[NPARNONROUTING+2*NPARROUTING+1] = K inflow 3    (sec)
    parameters[NPARNONROUTING+2*NPARROUTING+2] = x inflow 3   (-)
    ...
	(parameters routing inflow NINFLOWMAX)
    parameters[NPARNONROUTING+NINFLOWMAX*NPARROUTING] = lag inflow 3   (sec)
    parameters[NPARNONROUTING+NINFLOWMAX*NPARROUTING+1] = K inflow 3    (sec)
    parameters[NPARNONROUTING+NINFLOWMAX*NPARROUTING+2] = x inflow 3   (-)



double * states,
	states[0] = outflow            (m3/s)
    states[1] = overbank_flow,   (m3/s)
    states[2] = floodplain_volume, (m3)
    states[3] = floodplain_area,   (m2)
    states[4] = flood return flow  (m3/s)
    states[5] = river rainfall flux (m3/s)
    states[6] = river evap flux     (m3/s)
    states[7] = flood rainfall flux (m3/s)
    states[8] = flood evap flux     (m3/s)
   	states[9] = floodplain groundwater loss (m3/s)
   	states[10] = monod loss         (m3/s)
   	states[11] = anabranch loss - upstream of floodplain        (m3/s)
   	states[12] = anabranch loss - downstream of floodplain         (m3/s)
   	states[13] = previous reservoir volume         (m3)
   	states[14] = previous reservoir area         (m2)
   	states[15] = rainfall flux on reservoir         (m3/s)
   	states[16] = evap flux from reservoir         (m3/s)
   	states[17] = reservoir contribution         (m3/s)
   	states[18] = flux from reservoir to attributed to subsequent time steps        (m3/s)
   	states[19] = outflow 2        (m3/s)
   	states[20] = outflow 3        (m3/s)
   	states[21] = outflow 4        (m3/s)
   	states[22] = outflow 5        (m3/s)
   	states[23] = outflow 6        (m3/s)
   	states[24] = outflow 7        (m3/s)
   	states[25] = river volume     (m3)
	states[26] = floodplain groundwater max change in storage     (m3/s)
	states[27] = floodplain groundwater outflow     (m3/s)
	states[28] = floodplain groundwater maximum infiltration     (m3/s)
	states[29] = river groundwater max change in storage     (m3/s)
	states[30] = river groundwater outflow     (m3/s)
	states[31] = river groundwater maximum infiltration     (m3/s)
	states[32] = river groundwater maximum monod loss     (m3/s)

	(routing states, inflow 1)
   	states[NSTATESNONROUTING] = 	previous_inflow - inflow 1      (m3/s)
    states[NSTATESNONROUTING+1] =		routing_volume - inflow 1      (m3)
    states[NSTATESNONROUTING+2] =		sum_outflow - inflow 1         (m3/s)
    states[NSTATESNONROUTING+3] =		instantaneous_outflow - inflow 1  (m3/s)
    states[NSTATESNONROUTING+4] =		lag uh 1 - inflow 1            (m3/s)
    states[NSTATESNONROUTING+5] =		lag uh 2 - inflow 1            (m3/s)
	...
    states[NSTATESNONROUTING+NSTATESROUTING-1] =		lag uh NLAGMAX - inflow 1 (m3/s)

 	(routing states, inflow 2)
   	states[NSTATESNONROUTING+NSTATESROUTING] = 	previous_inflow - inflow 2
    states[NSTATESNONROUTING+NSTATESROUTING+1] =		routing_volume - inflow 2
    states[NSTATESNONROUTING+NSTATESROUTING+2] =		sum_outflow - inflow 1
    states[NSTATESNONROUTING+NSTATESROUTING+3] =		instantaneous_outflow - inflow 1
    states[NSTATESNONROUTING+NSTATESROUTING+4] =		lag uh 1 - inflow 2
    states[NSTATESNONROUTING+NSTATESROUTING+5] =		lag uh 2 - inflow 2
	...
    states[NSTATESNONROUTING+2*NSTATESROUTING-1] =		lag uh NLAGMAX - inflow 2

 	(routing states, inflow 3)
   	states[NSTATESNONROUTING+2*NSTATESROUTING] = 	previous_inflow - inflow 3
    states[NSTATESNONROUTING+2*NSTATESROUTING+1] =		routing_volume - inflow 2
    states[NSTATESNONROUTING+2*NSTATESROUTING+2] =		sum_outflow - inflow 1
    states[NSTATESNONROUTING+2*NSTATESROUTING+3] =		lag uh 1 - inflow 3
    states[NSTATESNONROUTING+2*NSTATESROUTING+4] =		lag uh 2 - inflow 3
	...
    states[NSTATESNONROUTING+3*NSTATESROUTING-1] =		lag uh NLAGMAX - inflow 3
	...
 	(routing states, inflow NINFLOWMAX)
   	states[NSTATESNONROUTING+NINFLOWMAX*NSTATESROUTING] = 	previous_inflow - inflow 3
    states[NSTATESNONROUTING+NINFLOWMAX*NSTATESROUTING+1] =		routing_volume - inflow 2
    states[NSTATESNONROUTING+NINFLOWMAX*NSTATESROUTING+2] =		sum_outflow - inflow 1
    states[NSTATESNONROUTING+NINFLOWMAX*NSTATESROUTING+3] =		lag uh 1 - inflow 3
    states[NSTATESNONROUTING+NINFLOWMAX*NSTATESROUTING+4] =		lag uh 2 - inflow 3
	
-- Authors --
Julien Lerat, CSIRO CLW
Shaun Kim, CSIRO CLW
Ang Yang, CSIRO CLW

-- Versions --
2012-10-23 - First version of the code
2014-01-30 - Update to include more floodplain and river groundwater parameters
2014-02-26 - update to include irrigation module

*******************************************************************************/


/******* Sub routines**********************************************************/
double max(double v1,double v2);
double min(double v1,double v2);
double minmax(double min,double max,double input);
double monod(double p1,double p2,double input);

void flood_runtimestep(int * ierr,		
		double timestep_length,
		double return_flow_coefficient,
		double floodplainKsat,
		double floodAlpha,
		double floodBeta,
		double floodGamma,
		double overbankflow_threshold,
		double Qup,
		double rainfall,
		double evap,
		double * overbank_flow,
		double * floodplain_volume,
		double * floodplain_area,
		double * return_flow,
		double * rainfall_flux,
		double * evap_flux,	
		double * groundwater_loss,		
		double depthToGw,
		double aquiferSpecificYield,
		double surfaceLayerThickness,
		double aquiferKsat,
		double aquiferSaturatedThickness,
		double floodplainLength,
		double * floodplain_gw_max_change_storage,
		double * floodplain_gw_outflow,
		double * floodplain_gw_max_infiltration);

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
	);

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
		int julianDay,
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
		double * diversionCarryOver,		//(m3/s)
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

/******* Main code ************************************************************/
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
		double * useDeadStorage)
{
    // Dimensions
    int i,k,ninflows,use_routing,use_flood,use_monod,
      use_reservoir,use_ungauged,use_anabranch,debugflag;

    // config data
	double timestep_length,floodAlpha,floodBeta,floodGamma,riverAlpha,riverBeta,
            overbankflow_threshold,anabloss_top_a,anabloss_top_b,
            anabloss_bottom_a,anabloss_bottom_b,river_length,
			riverDepthAlpha,riverDepthBeta;

    // parameters
	double return_flow_coefficient,floodplainKsat,lag,
		monod1,monod2,K,x,runoff_correction_factor,
		aquiferSpecificYield,aquiferKsat,aquiferSaturatedThickness,
		surfaceLayerThickness,riverConductance;

    // input
	double rainfall_river=0.0, evap_river=0.0,
          rainfall_floodplain=0.0, evap_floodplain=0.0,
          irrigation_diversion=0.0,irrigation_returnflow,inflow=0.0,
          ungauged_inflow_top=0.0,ungauged_inflow_bottom=0.0,urban_diversion=0.0,
          reservoir_volume=0.0,reservoir_area=0.0,depthToGw=0.0,river_depth=0.0,river_width=0.0,
		  reservoir_net_diversion=0.0,other_river_diversion=0.0;

   // Other variables
   double river_area=0.0,fact=0.0,flux,flux1,flux2,ratio;
   double infiltPotentialRiver=0.0,totalStorageAvailRiver,QaquiferRiver,totalStorageAvailPlQaquiferRiver;
   double riverWaveDuration=1.0; // riverWaveDuration=timestep_length
   double Qtemp,river_area_temp;
   double headwater_river_volume_estimation,estim_river_depth;
   double Xalv,Dalv,reach_area; //SYalv
   double alpla_alv,beta_alv,gamma_alv,Salv;
   
   // dead storage
   double dead_storage_length;
   double dead_storage_max_width;
   double dead_storage_max_height;
   
   double dead_storage_alpha=1.0;
   double dead_storage_water_max_xsection_area;
   double dead_storage_max_volume=0.0;
   double dead_storage_volume;
   
   double dead_storage_xsection_area;
   double dead_storage_water_height;
   double dead_storage_water_width;
   double dead_storage_surface_area;
   double dead_storage_flux;
   
    debugflag = 0;

    // Check dimensions
    ninflows		= *ninputs-NINPUTSNONROUTING;
    if(ninflows<0)
    {
        *ierr = 50100101;
        return;
    }
    if(*nstates != NSTATESNONROUTING + NSTATESROUTING*ninflows)
    {
        *ierr = 50100102;
        return;
    }
    if(*npar != NPARNONROUTING + NPARROUTING*ninflows)
    {
        *ierr = 50100103;
        return;
    }
    if(*nconfig < NCONFIG)
    {
        *ierr = 50100104;
        return;
    }

   // Set config
	use_routing            = (int) config[0];
	use_flood              = (int) config[1];
	use_monod              = (int) config[2];
	use_reservoir          = (int) config[3];
	use_ungauged           = (int) config[4];
	use_anabranch          = (int) config[5];

	timestep_length        = config[6],
  timestep_length = minmax(1.0,LARGEVALUE,timestep_length);

	riverAlpha             = config[7];
  riverAlpha = minmax(0.0,LARGEVALUE,riverAlpha);

	riverBeta              = config[8];
  riverBeta = minmax(0.0,10.0,riverBeta);
  
	floodAlpha             = config[9];
  floodAlpha = 0.0; // minmax(0,LARGEVALUE,floodAlpha); - floodAlpha is not used 

	floodBeta              = config[10];
  floodBeta = minmax(0.45,100.0,floodBeta); // average depth varying from 1cm to 10m

	anabloss_top_a         = config[11];
  anabloss_top_a = minmax(0.0,10.0,anabloss_top_a);

	anabloss_top_b         = config[12];
  anabloss_top_b = minmax(0.1,1.0,anabloss_top_b);

	anabloss_bottom_a      = config[13];
  anabloss_bottom_a = minmax(0.0,10.0,anabloss_bottom_a);

	anabloss_bottom_b      = config[14];
  anabloss_bottom_b = minmax(0.1,1.0,anabloss_bottom_b);
  
    river_length       = config[15];
  river_length = minmax(1.0,LARGEVALUE,river_length);
  
	riverDepthAlpha             = config[16];
  riverDepthAlpha = minmax(0.0,LARGEVALUE,riverDepthAlpha);

	riverDepthBeta              = config[17];
  riverDepthBeta = minmax(0.0,10.0,riverDepthBeta);
  
  Xalv              = config[18];
  if(*useDynMax>0.5){
    Xalv = minmax(0.0001,LARGEVALUE,Xalv);
  } else {
    Xalv = minmax(0.0,LARGEVALUE,Xalv);
  }
  
  
  Dalv              = config[19];
  Dalv = minmax(0.0,LARGEVALUE,Dalv);
  
  //SYalv              = config[20];
  //SYalv = minmax(0,LARGEVALUE,SYalv);
  
  reach_area              = config[20];
  reach_area = minmax(0.0,LARGEVALUE,reach_area);
  
  dead_storage_length              = config[21];
  dead_storage_length = minmax(0.0,LARGEVALUE,dead_storage_length);
  
  dead_storage_max_width              = config[22];
  dead_storage_max_width = minmax(0.0,LARGEVALUE,dead_storage_max_width);
  
  dead_storage_max_height              = config[23];
  dead_storage_max_height = minmax(0.0,LARGEVALUE,dead_storage_max_height);
  
  // states[33] is Salv
		
    // Set parameters other than Muskingum parameters
	return_flow_coefficient 	= parameters[0];
  return_flow_coefficient = minmax(0.0,LARGEVALUE,return_flow_coefficient);

	floodplainKsat                    	= parameters[1];
  floodplainKsat = minmax(0.0,1e-4,floodplainKsat);  // floodplainKsat [0 - 100 mu.m/s]

	monod1                   	= parameters[2];
  monod1 = minmax(0.0,LARGEVALUE,monod1);

	monod2                   	= parameters[3];
  monod2 = minmax(0.0,LARGEVALUE,monod2);

  overbankflow_threshold 		= parameters[4];
  overbankflow_threshold = minmax(1e-1,LARGEVALUE,overbankflow_threshold);     // Miminum overbank flow threshold set to 0.1 m3/s
  
	floodGamma             		= parameters[5];
  floodGamma = minmax(0.1,0.9,floodGamma);   // Overbank flow threshold between [0.1 - 0.9]

  runoff_correction_factor = parameters[6];
  runoff_correction_factor = minmax(0.0,LARGEVALUE,runoff_correction_factor);

  aquiferSpecificYield = parameters[7];
  aquiferSpecificYield = minmax(0.0,LARGEVALUE,aquiferSpecificYield);
  
  aquiferKsat = parameters[8];
  aquiferKsat = minmax(0.0,LARGEVALUE,aquiferKsat);
  
  aquiferSaturatedThickness = parameters[9];
  aquiferSaturatedThickness = minmax(0.0,LARGEVALUE,aquiferSaturatedThickness);
  
  surfaceLayerThickness = parameters[10];
  surfaceLayerThickness = minmax(0.0001,LARGEVALUE,surfaceLayerThickness);
  
  riverConductance = parameters[11];
  riverConductance = minmax(0.0,LARGEVALUE,riverConductance);
  
  alpla_alv = parameters[12];
  alpla_alv = minmax(0.0,1.0,alpla_alv);
  
  beta_alv = parameters[13];
  beta_alv = minmax(0.0001,LARGEVALUE,beta_alv);
  
  gamma_alv = parameters[14];
  gamma_alv = minmax(0.0,LARGEVALUE,gamma_alv);
  
  // Set inputs apart from inflows
  rainfall_river        = inputs[0];
  rainfall_river = minmax(0.0,5e2,rainfall_river);

  evap_river            = inputs[1];
  evap_river = minmax(0.0,5e1,evap_river);

  rainfall_floodplain   = inputs[2];
  rainfall_floodplain = minmax(0.0,5e2,rainfall_floodplain);

  evap_floodplain       = inputs[3];
  evap_floodplain = minmax(0.0,5e1,evap_floodplain);

  irrigation_diversion  = inputs[4];
  irrigation_diversion = minmax(0.0,5e2,irrigation_diversion);

  irrigation_returnflow = inputs[5];
  irrigation_returnflow = minmax(0.0,5e2,irrigation_returnflow);

  urban_diversion       = inputs[6];
  urban_diversion = minmax(0.0,5e2,urban_diversion);

  ungauged_inflow_top   = inputs[7];
  ungauged_inflow_top = minmax(0.0,LARGEVALUE,ungauged_inflow_top);

  ungauged_inflow_bottom= inputs[8];
  ungauged_inflow_bottom = minmax(0.0,LARGEVALUE,ungauged_inflow_bottom);

  reservoir_volume      = inputs[9];
  reservoir_volume = minmax(0.0,1e13,reservoir_volume);

  reservoir_area        = inputs[10];
  reservoir_area = minmax(0.0,LARGEVALUE,reservoir_area);
  
  depthToGw             = inputs[11];
  depthToGw = minmax(-LARGEVALUE,LARGEVALUE,depthToGw);
  
  river_depth           = inputs[12];
  river_depth = minmax(-1.0,LARGEVALUE,river_depth);
  
  river_width           = inputs[13];
  river_width = minmax(-1.0,LARGEVALUE,river_width);
  
  reservoir_net_diversion           = inputs[14];
  reservoir_net_diversion = minmax(-LARGEVALUE,LARGEVALUE,reservoir_net_diversion);
  
  other_river_diversion             = inputs[15];
  other_river_diversion = minmax(-LARGEVALUE,LARGEVALUE,other_river_diversion);
	
   // Initialise outflow
  states[0] = 0.0;
  
  // dead storage
  if(*useDeadStorage>0.5){
    //printf("dead_storage_length=%0.2f\n ",dead_storage_length);
    //printf("dead_storage_max_width=%0.2f\n ",dead_storage_max_width);
    //printf("dead_storage_max_height=%0.2f\n ",dead_storage_max_height);
    dead_storage_alpha = 4.0 * dead_storage_max_height / pow(dead_storage_max_width,2.0);
    //printf("dead_storage_alpha=%0.2f\n ",dead_storage_alpha);
    dead_storage_water_max_xsection_area = 2.0 / 3.0 * dead_storage_max_width * dead_storage_max_height;
    //printf("dead_storage_max_width=%0.2f\n ",dead_storage_max_width);
    //printf("dead_storage_max_height=%0.2f\n ",dead_storage_max_height);
    //printf("dead_storage_water_max_xsection_area=%0.2f\n ",dead_storage_water_max_xsection_area);
    dead_storage_max_volume = dead_storage_water_max_xsection_area * dead_storage_length;
    //printf("dead_storage_max_volume=%0.2f\n ",dead_storage_max_volume);
    
  }
  
  dead_storage_volume = states[38];

  
  // Alluvium as River Storage loss

  double alv_area;
  double SMalv;
  double runoff_alv;
  double rainfall_alv;
  double evap_alv;
  double rainvol_alv;
  double evapvol_alv;

  
  // Loop through tributaries - route inflows
  // (ninflow x current inflow values + ninflow x previous inflow)
	for(i=0;i<ninflows;i++)
	{
		inflow = 0.0;
		if(inputs[i+NINPUTSNONROUTING]>0.0)
				inflow = inputs[i+NINPUTSNONROUTING];

		// Routing parameters
		lag	= parameters[i*NPARROUTING+NPARNONROUTING];
		K 	= parameters[1+i*NPARROUTING+NPARNONROUTING];
		x 	= parameters[2+i*NPARROUTING+NPARNONROUTING];

		if(use_routing==1)
		{
			// lag subroutine
			lag_runtimestep(ierr,timestep_length,
				inflow,lag,
				&(states[4+i*NSTATESROUTING+NSTATESNONROUTING]));

			// Routing subroutine
			muskingum_runtimestep(ierr,
				  timestep_length,
				  states[4+i*NSTATESROUTING+NSTATESNONROUTING],
				  K,
				  x,
				  &(states[i*NSTATESROUTING+NSTATESNONROUTING]),
				  &(states[1+i*NSTATESROUTING+NSTATESNONROUTING]),
				  &(states[2+i*NSTATESROUTING+NSTATESNONROUTING]),
				  &(states[3+i*NSTATESROUTING+NSTATESNONROUTING]));

			// Sum routing contributions
			states[0]+=states[3+i*NSTATESROUTING+NSTATESNONROUTING];
			states[25]+=(inflow-states[3+i*NSTATESROUTING+NSTATESNONROUTING])*timestep_length; // river volume
			if(states[25]<0.0)
			{
        // takes water from outflow to maintain mass balance
			  states[0]+=states[25];
		    // in case there is not enough water in outflow
		    if(states[0]<0.0){
		      states[0] = 0.0;
		    }
				states[25] = 0.0;
			}
			
		}
		else
		{
			states[0]+=inflow; // do not use routing.
			//states[25]+=inflow*timestep_length; // river volume
			states[25] = 0.0; // river volume
		}
	}

  // Ungauged inflows - top
  if(use_ungauged==1)
	{
		states[0]+= ungauged_inflow_top*runoff_correction_factor;
	}

  Salv = states[33];
  
  // work out river width
  river_area_temp = riverAlpha*pow(states[0], riverBeta);
  double river_width_temp = river_area_temp / river_length;
  //if(timestep_index==9068){
  //  int ii;
  //  for(ii=0;ii<100;ii++){
  //    printf("river_area_temp=%0.5f riverAlpha=%0.5f states[0]=%0.5f riverBeta=%0.5f river_width_temp=%0.5f river_length=%0.5f \n",
  //           river_area_temp,riverAlpha,states[0],riverBeta,river_width_temp,river_length);
  //  }
  //}
  //double state0_1_tmp = states[0];
  
  // Alluvium as River Storage loss
  if(*useDynMax>0.5){
    alv_area = max(0.0,(Xalv + river_width_temp) * river_length - river_area_temp);
    SMalv = (Xalv + river_width_temp) * Dalv * aquiferSpecificYield * river_length;
  } else {
    alv_area = max(0.0,Xalv * river_length - river_area_temp);
    SMalv = Xalv * Dalv * aquiferSpecificYield * river_length;
  }
  
  // this overrides depthToGw input
  depthToGw = Dalv * (1.0 - Salv/SMalv);
  
  if(*useDynMax>0.5){
    if(depthToGw<0.0){
      depthToGw = 0.0;
    }
  }
  
  
  // override input aquiferSaturatedThickness
  aquiferSaturatedThickness = Dalv * (Salv/SMalv);
  if(*useDynMax>0.5){
    // this assumes qaquifer is not impacted by excess water in alv
    if(aquiferSaturatedThickness>Dalv){
      aquiferSaturatedThickness = Dalv;
    }
  }
  
  states[34] = depthToGw;
  
  runoff_alv = (ungauged_inflow_top*runoff_correction_factor)/reach_area; // m/s
  rainfall_alv = rainfall_river * 1e-3/timestep_length; // m/s
  evap_alv = evap_river * 1e-3/timestep_length; // m/s
  rainvol_alv = 0.0;
  if(rainfall_alv>runoff_alv)
  {
    rainvol_alv = alpla_alv * (rainfall_alv - runoff_alv) * alv_area * timestep_length; // m^3/timestep
  }
  evapvol_alv = evap_alv * pow(exp(-depthToGw/beta_alv),gamma_alv) * alv_area * timestep_length;
  
  //printf(" calc  rainvol_alv=%0.2f\n ",rainvol_alv);
  //printf(" calc  evapvol_alv=%0.2f\n ",evapvol_alv);
  
  
    // Reservoir
    if(use_reservoir==1)
    {
	
        // Reservoir model uses the same rainfall/evap than river
        reservoir_runtimestep(ierr,
      		timestep_length,
      		states[0],
      		reservoir_volume,
      		reservoir_area,
      		rainfall_river,
      		evap_river,
      		&(states[13]),
      		&(states[14]),
      		&(states[15]),
      		&(states[16]),
      		&(states[17]),
      		&(states[18]),
			reservoir_net_diversion);
			
		  flux = states[17];

      	  states[0] += flux; // outflow

    }
	
    // Ungauged inflows - bottom
    if(use_ungauged==1)
	{
		states[0]+= ungauged_inflow_bottom*runoff_correction_factor;
	} 

	//irrigation module
	if(*use_irrig==1)
	{
		//initialise irrigation module
		double timestep, areaIrrig_max, licVol, volOFSmax, volGWmax, efficiency, returnflow_coef, soilCap, gamma, sigma, ringtankAvgDepth,syAq,kSurf,
		alpha_irrig, beta_irrig, threshOFS, maxPump, pumpAdjust,
		rainfall, evap, allocation, weighted_Kc, areaActivePro, julianDay, reachInflow, irrigDepthToGW;

		//initilise irrigation config
		timestep        = timestep_length;   //irrigConfig[0] is superseeded by timestep_length

		areaIrrig_max 		= irrigConfig[1]; // Max irrigated area (km2)
		//areaIrrig_max = minmax(0,1e11,areaIrrig_max); // Max = 1e11 m2 = 10 000 Ha

		licVol 				= irrigConfig[2];
		//licVol = minmax(0,1e6,licVol); // Max = 1e6 m3 = 1000 GL

		volOFSmax 			= irrigConfig[3];
		//volOFSmax = minmax(0,1e6,volOFSmax); // Max = 1e6 m3 = 1000 GL

		volGWmax 				= irrigConfig[4];
		//volGWmax = minmax(0,1e6,volGWmax); // Max = 1e6 m3 = 1000 GL

		efficiency 			= irrigConfig[5];
		//efficiency = minmax(0,10,efficiency); // I don't understand why efficieny >1 ?

		returnflow_coef = irrigConfig[6];
		returnflow_coef = minmax(0,1,returnflow_coef);

		soilCap = irrigConfig[7];
		gamma = irrigConfig[8];
		sigma = irrigConfig[9];
		ringtankAvgDepth = irrigConfig[10];

		// new config items here (November 2013)                      ****NEW****
		syAq   = irrigConfig[11]; // new config item specific yield aquifer  (dimensionless, range=0 - 0.5) ****NEW****
		kSurf = irrigConfig[12]; // new config item, surface layer conductivity (m/s) ****NEW****

		//initialise irrigation parameters
		alpha_irrig = irrigParameters[0];
		//alpha_irrig = minmax(0,1e30,alpha_irrig);

		beta_irrig = irrigParameters[1];
		//beta_irrig = minmax(0,1e30,beta_irrig);

		threshOFS = irrigParameters[2];
		//threshOFS = minmax(0,1e30,threshOFS);

		maxPump = irrigParameters[3];
		//maxPump = minmax(0,1e30,maxPump);

		pumpAdjust = irrigParameters[4];
		//pumpAdjust = minmax(0,1e30,pumpAdjust);

		//initialise irrigation inputs
		rainfall        = rainfall_river;	//irrigInputs[0] is superseeded by rainfall_river
		evap            = evap_river;	//irrigInputs[1] is superseeded by evap_river

		irrigInputs[0] = rainfall;
		irrigInputs[1] = evap;
		
		allocation  = irrigInputs[2];
		allocation = minmax(0,1,allocation); // Allocation within 0 to 100%

		weighted_Kc = irrigInputs[3];
		//weighted_Kc = minmax(0,5,weighted_Kc); // Max KC set to 5. Could be lower, probably 2

		areaActivePro = irrigInputs[4];
		areaActivePro = minmax(0,1,areaActivePro); // Proportion

		julianDay = irrigInputs[5];
		julianDay = minmax(1,366,julianDay);

		reachInflow = states[0];	//irrigInputs[6] is superseeded by generated outflow states[0]
		irrigInputs[6] = reachInflow;
		
		irrigDepthToGW = depthToGw; // irrigInputs[7] depth to groundwater time series (m) is superseeded by depthToGW
		irrigInputs[7] = irrigDepthToGW; 
	
		irrigation_runtimestep(
			ierr, timestep,
			soilCap,gamma,sigma,
			areaIrrig_max,licVol,volOFSmax,volGWmax,efficiency,returnflow_coef,ringtankAvgDepth,syAq,kSurf,
			reachInflow,allocation,weighted_Kc,areaActivePro,julianDay,rainfall,evap,irrigDepthToGW,
			alpha_irrig,beta_irrig,threshOFS,maxPump,pumpAdjust,
			&(irrigStates[0]),&(irrigStates[1]),&(irrigStates[2]),
			&(irrigStates[3]),&(irrigStates[4]),&(irrigStates[5]),
			&(irrigStates[6]),&(irrigStates[7]),&(irrigStates[8]),
			&(irrigStates[9]),&(irrigStates[10]),&(irrigStates[11]),
			&(irrigStates[12]),&(irrigStates[13]),&(irrigStates[14]),
			&(irrigStates[15]),&(irrigStates[16]),&(irrigStates[17]),
			&(irrigStates[18]),&(irrigStates[19]),
			&(irrigStates[20]),&(irrigStates[21]),&(irrigStates[22]));	
	
		irrigation_diversion = irrigStates[7] + irrigStates[12];	//OFSin + Diversion
		irrigation_returnflow = irrigStates[10] + irrigStates[14];	//Runoff + Drainage
		
		// only use irrigation model values if input diversion is negative
		if(inputs[4] < 0.0 || inputs[5] < 0.0)
		{
			inputs[4] = irrigation_diversion;
			inputs[5] = irrigation_returnflow;
		}
		else
		{
			irrigation_diversion = inputs[4];
			irrigation_returnflow = inputs[5];
		}
		
	}
	
    // Town water supply and irrigation diversion
    flux =   irrigation_diversion - irrigation_returnflow + urban_diversion + other_river_diversion;
	if(states[0]<flux) flux = states[0];
   	states[0]-= flux; // outflow

 	// Dead storage
 	if(*useDeadStorage>0.5){
 	  dead_storage_volume += states[0] * timestep_length;
 	  double apply_dead_storage_fluxes;
 	  if(dead_storage_volume > dead_storage_max_volume){
 	    states[0] = (dead_storage_volume - dead_storage_max_volume)/timestep_length;
 	    dead_storage_volume = dead_storage_max_volume;
 	    apply_dead_storage_fluxes = 0.0;
 	  } else {
 	    states[0] = 0.0;
 	    apply_dead_storage_fluxes = 1.0;
 	  }
 	  
 	  dead_storage_xsection_area = dead_storage_volume/dead_storage_length;
 	  dead_storage_water_height = pow(3.0 / 4.0 * dead_storage_xsection_area * pow(dead_storage_alpha,0.5),2.0/3.0);
 	  dead_storage_water_width = 2.0 * pow(dead_storage_water_height/dead_storage_alpha,0.5);
 	  dead_storage_surface_area = dead_storage_water_width*dead_storage_length;
 	  dead_storage_flux = (evap_river - rainfall_river) * 1e-3; // m/timestep
 	  //double dead_storage_flux = 0.0;
 	  
 	  // Only perform dead storage rain and evap fluxes if flow is zero i.e. dead storage is not filled up
 	  if(apply_dead_storage_fluxes>0.5){
 	    dead_storage_volume -= dead_storage_flux*dead_storage_surface_area;
 	  }
 	  
 	  if(dead_storage_volume > dead_storage_max_volume){
 	    states[0] += (dead_storage_volume - dead_storage_max_volume)/timestep_length;
 	    dead_storage_volume = dead_storage_max_volume;
 	  }
 	  
 	  if(dead_storage_volume < 0.0){
 	    dead_storage_volume = 0.0;
 	  }
 	  
 	  states[38] = dead_storage_volume;
 	}
   	
	// Rainfall and evap fluxes if routing switched on
	states[5] = 0.0;
	states[6] = 0.0;
	if(use_routing==1)
	{
		river_area = riverAlpha*pow(states[0], riverBeta);
	  	states[5] = rainfall_river * 1e-3/timestep_length *  river_area;
	  	states[6] = evap_river * 1e-3/timestep_length * river_area;
		flux = states[6]-states[5];
	  	if(flux<states[0])
		  	states[0]-= flux;
	  	else
	  	{
			fact=0.0;
			if(flux>0) fact = states[0]/flux;
			states[5]*=fact;
			states[6]*=fact;
			states[0]=0.0;
	  	}
	  	
	}

    // Anabranches
    if(use_anabranch==1)
	   {
      flux1 =  pow(states[0],anabloss_top_b)*anabloss_top_a;
      if(flux1<0) flux1 = 0.0;
  	  flux2 =  pow(states[0],anabloss_bottom_b)*anabloss_bottom_a;
      if(flux2<0) flux2 = 0.0;
  
      // Check that there is enough water
  	  ratio =0.0;
  	  if(states[0]>0.0) ratio = (flux1+flux2)/states[0];
  	  if(ratio>1.0)
      {
        flux1 /= ratio; 
        flux2 /= ratio; 
      }
  	
      states[11] = flux1;
      states[12] = flux2;
	  flux = max(0.0,states[0]-flux1-flux2);
	  states[0] = flux;
	   }
 
    // Apply floodplain model  (states[0] = Qup)
    if(use_flood==1)
    {
		flood_runtimestep(ierr,
			timestep_length,
			return_flow_coefficient,
			floodplainKsat,
			floodAlpha,
			floodBeta,
			floodGamma,
			overbankflow_threshold,
			states[0],
			rainfall_floodplain,
			evap_floodplain,
			&(states[1]),
			&(states[2]),
			&(states[3]),
			&(states[4]),
			&(states[7]),
			&(states[8]),
			&(states[9]),
			depthToGw,
			aquiferSpecificYield,
			surfaceLayerThickness,
			aquiferKsat,
			aquiferSaturatedThickness,
			river_length,
			&(states[26]),
			&(states[27]),
			&(states[28]));

		// Final outflow = routed flow - overbank flow
		flux = states[1]-states[4];
		if(states[0]<flux) flux = states[0];
      	states[0]-= flux;
    }
  
	// Monod loss
	states[10] = 0.0;
	//if(monod2+states[0]>0) states[10] = monod(monod1,monod2,states[0]);
	//if(states[0]<states[10]) states[10] = states[0];
	states[32] = states[10];
	
	// printf("states[10]=%0.5f monod1=%0.5f monod2=%0.5f states[0]=%0.5f \n",
	// states[10],monod1,monod2,states[0]);
	
  	if(use_monod==1)
	{
		// work out river depth
		Qtemp = states[0];
		river_depth = pow(Qtemp / riverDepthAlpha,1.0/riverDepthBeta);
		
		// work out river width
		river_area_temp = riverAlpha*pow(states[0], riverBeta);
		river_width = river_area_temp / river_length;

		// limit to groundwater loss
		infiltPotentialRiver = riverConductance * river_width * (river_depth / surfaceLayerThickness + 1.0) * river_length * riverWaveDuration;
		if(depthToGw>0.0)
		{
		  // divided by timestep_length to make this a maximum RATE
			totalStorageAvailRiver = depthToGw * aquiferSpecificYield * river_width * river_length / timestep_length;
		}
		else
		{
			totalStorageAvailRiver = 0.0;
		}
		
		if(river_width<1e-6)
		{
			QaquiferRiver = 0.0;
		}
		else
		{
			QaquiferRiver = aquiferKsat * aquiferSaturatedThickness * river_length * riverWaveDuration * (river_depth / (river_width/2.0));

		}
		totalStorageAvailPlQaquiferRiver = totalStorageAvailRiver + QaquiferRiver;
		
		if(infiltPotentialRiver<totalStorageAvailPlQaquiferRiver)
		{
		  flux = infiltPotentialRiver;
		}
		else
		{
		  flux = totalStorageAvailPlQaquiferRiver;
		}
		
		if(flux<0.0)
		{
			flux = 0.0;
		}
		
		states[29] = totalStorageAvailRiver;
		states[30] = QaquiferRiver;
		states[31] = infiltPotentialRiver;
		
		if(states[0]<flux) flux = states[0];
    states[0]-= flux;
		
		// printf("infiltPotentialRiver=%0.5f totalStorageAvailPlQaquiferRiver=%0.5f states[10]=%0.5f \n",
		// infiltPotentialRiver,totalStorageAvailPlQaquiferRiver,states[10]);
		
		// compute alluvium fluxes - one at a time for mass balance reasons
		double prev_Salv=Salv;
		double Salv_tmp=0.0;
		
		if(Salv>=SMalv && *useDynMax>0.5){
		  states[0] += flux;
		  flux = 0.0;
		} else {
		  Salv_tmp = Salv + flux*timestep_length;
		  if(Salv_tmp>SMalv){
		    states[0] += (Salv_tmp-SMalv)/timestep_length;
		    flux = (SMalv-Salv)/timestep_length;
		    Salv = SMalv;
		  } else {
		    Salv = Salv_tmp;
		  }
		}
		

		if(Salv>=SMalv && *useDynMax>0.5){
		  // this is for the shrinking alluvium capacity case and does not produce excess runoff
		  rainvol_alv = 0.0;
		} else {
		  Salv_tmp = Salv + rainvol_alv;
		  if(Salv_tmp>SMalv){
		    states[0] += (Salv_tmp-SMalv)/timestep_length;
		    rainvol_alv = SMalv-Salv;
		    Salv = SMalv;
		  } else {
		    Salv = Salv_tmp;
		  }
		}

		states[35] = rainvol_alv;
		
		Salv_tmp = Salv - evapvol_alv;
		if(Salv_tmp<0.0){
		  evapvol_alv = Salv;
		  Salv = 0.0;
		} else {
		  Salv = Salv_tmp;
		}
		states[36] = evapvol_alv;
		
		// calc QaquiferRiver at end TS to prevent oscillation
		if(river_width<1e-6)
		{
		  QaquiferRiver = 0.0;
		} else {
		  double aquiferSaturatedThicknessTemp = Dalv * (Salv/SMalv);
		  if(*useDynMax>0.5){
		    // this assumes qaquifer is not impacted by excess water in alv
		    if(aquiferSaturatedThicknessTemp>Dalv){
		      aquiferSaturatedThicknessTemp = Dalv;
		    }
		  }
		  QaquiferRiver = aquiferKsat * aquiferSaturatedThicknessTemp * river_length * riverWaveDuration * (river_depth / (river_width/2.0));
		}
		
		Salv_tmp = Salv - QaquiferRiver*timestep_length;
		if(Salv_tmp<0.0){
		  QaquiferRiver = Salv/timestep_length;
		  Salv = 0.0;
		} else {
		  Salv = Salv_tmp;
		}
		states[37] = QaquiferRiver;
		
		states[33] = Salv;
		states[10] = flux;
		
		double mass_balance = prev_Salv + flux*timestep_length + rainvol_alv - evapvol_alv - QaquiferRiver*timestep_length - Salv;
		
		if(mass_balance>1.0 || mass_balance<-1.0){
		  
		  printf("mass_balance=%0.2f\n ",mass_balance);
		  printf("  prev_Salv=%0.2f\n ",prev_Salv);
		  printf("  flux*timestep_length=%0.2f\n ",flux*timestep_length);
		  printf("  rainvol_alv=%0.2f\n ",rainvol_alv);
		  printf("  evapvol_alv=%0.2f\n ",evapvol_alv);
		  printf("  QaquiferRiver*timestep_length=%0.2f\n ",QaquiferRiver*timestep_length);
		  printf("  Salv=%0.2f\n ",Salv);
		  
		  printf("  alpla_alv=%0.2f\n ",alpla_alv);
		  printf("  rainfall_alv=%0.2f\n ",rainfall_alv);
		  printf("  runoff_alv=%0.2f\n ",runoff_alv);
		  //printf("  alv_area=%0.2f\n ",alv_area);
		  
		  printf("  evap_alv=%0.2f\n ",evap_alv);
		  printf("  beta_alv=%0.2f\n ",beta_alv);
		  printf("  Salv/SMalv=%0.2f\n ",Salv/SMalv);
		  printf("  gamma_alv=%0.2f\n ",gamma_alv);
		  printf("  alv_area=%0.2f\n ",alv_area);
		  
		}
	}
  	
 	// Multiple outlets
   	states[19] = states[11]; // outflow 2
   	states[20] = states[12]; // outflow 3
   	states[21] = 0; // outflow 4
   	states[22] = 0; // outflow 5
   	states[23] = 0; // outflow 6
   	states[24] = 0; // outflow 7
	
	// This is for headwaters so that fluxes and states are estimated but the outflow is just runoff * factor
	if(*is_headwater==1)
	{
		if(use_reservoir==0)
		{
			states[0]= (ungauged_inflow_top + ungauged_inflow_bottom) * runoff_correction_factor;
		}
		
		if(use_monod==1)
		{
			if(river_depth<0)
			{
				river_depth = pow(states[0] / riverDepthAlpha,1/riverDepthBeta);
			}
			estim_river_depth = pow(states[0] / riverDepthAlpha,1/riverDepthBeta);

			if(river_width<0)
			{
				river_area_temp = riverAlpha*pow(states[0], riverBeta);
				river_width = river_area_temp / river_length;
			}
			
			// river volume estimation for headwaters
			// Q = f(D) and A = f(Q) so we derive A = f(D)
			// then we integrate f(D) to get V = integral(f(D))
			// ie Q = depthAlpha * D^depthBeta
			// A = areaAlpha * (depthAlpha * D^depthBeta)^areaBeta
			// V = integral(areaAlpha * (depthAlpha * D^depthBeta)^areaBeta)
			// V = areaAlpha * depthAlpha^areaBeta * (D^(depthBeta * areaBeta +1))/(depthBeta * areaBeta +1)
			
			headwater_river_volume_estimation = riverAlpha * pow(riverDepthAlpha,riverBeta) * pow(estim_river_depth,riverDepthBeta * riverBeta + 1)/(riverDepthBeta * riverBeta + 1);
			states[25] = headwater_river_volume_estimation;
			
			// printf("headwater_river_volume_estimation=%0.5f riverAlpha=%0.5f riverBeta=%0.5f riverDepthAlpha=%0.5f riverDepthBeta=%0.5f river_depth=%0.5f \n",
			// headwater_river_volume_estimation,riverAlpha,riverBeta,riverDepthAlpha,riverDepthBeta,river_depth);
			
			// FILE *f = fopen("C:/Users/kim079/Documents/WIRADA/AWRA2014/Rscripts/new_headwater_test/testing/output.river.csv", "a");
			// fprintf(f,"%0.5f,%0.5f,%0.5f,%0.5f,%0.5f,%0.5f \n",
			// headwater_river_volume_estimation,river_depth,river_length,river_width,states[0],estim_river_depth);
			// fclose(f);
		}
	}

 	if(debugflag>1)
	{
		printf("\n\t\t-> config = ");
		for(k=0;k<*nconfig;k++) printf("%0.2f ",config[k]);

		printf("\n\t\t-> parameters = ");
		for(k=0;k<*npar;k++) printf("%0.2f ",parameters[k]);
		printf("\n\t\t-> inputs = ");
		for(k=0;k<*ninputs;k++) printf("%0.2f ",inputs[k]);

		printf("\n\t\t-> states (non routing) = ");
		for(k=0;k<NSTATESNONROUTING;k++)
				printf("%0.2f (%d) ",states[k],k);

		printf("\n\t\t-> states (routing-nonlag) = ");
		for(k=NSTATESNONROUTING;k<4+NSTATESNONROUTING;k++)
				printf("%0.2f (%d) ",states[k],k);

		printf("\n\t\t-> states (routing-lag) = ");
		for(k=NSTATESNONROUTING+4;k<NSTATESROUTING+NSTATESNONROUTING;k++)
				printf("%0.2f (%d) ",states[k],k);

		printf("\n\n");
	}

  return;
}
