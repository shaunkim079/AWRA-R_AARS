#include "header.h"

/******************************************************************************
Flood modelling component of AWRAR

-- inputs ---
double timestep_length,				Time step length (seconds)
double return_flow_coefficient,		Coefficient used to compute return flow from flood volume
double floodplainKsat,				Floodplain saturated conductivity    (m/s)
double floodAlpha,					Vol/Area relationship parameter 1 (m^-1)
double floodBeta,					Vol/Area relationship parameter 2 (m2)
double floodGamma,					Exponent used to compute the overbank flow threshold
double overbankflow_threshold		Overbank flow threshold
double rainfall,					Rainfall over time step (mm/d)
double evap,						evap over time step (mm/d)
double overbank_flow,				Flow river -> floodplain (m3/s)
double depthToGw,					Depth to groundwater (m)
double aquiferSpecificYield,		Aquifer specific yield
double surfaceLayerThickness,		Surface layer thickness (m)
double aquiferKsat,					Aquifer saturated conductivity (m/s)
double aquiferSaturatedThickness,	Aquifer saturated thickness (m)
double floodplainLength,			Length of floodplain (m)

-- states ---
double * floodplain_volume,			Volume (m3)
double * floodplain_gw_max_change_storage,			floodplain groundwater max change in storage (m3/s)
double * floodplain_gw_outflow,			floodplain groundwater outflow (m3/s)
double * floodplain_gw_max_infiltration			floodplain groundwater maximum infiltration (m3/s)

-- outputs ---
double * floodplain_area,			Area (m2)
double * return_flow,				Flow floodplain -> river (m3/s)
double * rainfall_flux,				Rainfall flux (m3/s)
double * evap_flux					evap flux (m3/s)

-- Authors --
Julien Lerat, CSIRO CLW
Justin Hughes, CSIRO CLW
Shaun Kim, CSIRO CLW

-- Versions --
2012-10-23 - First translation of Justin's fortran code
2014-01-30 - Update to include more floodplain groundwater parameters

-- Original fortran code --

c sample subroutine to include for R
       SUBROUTINE flood(xx,Vfp,Qx,Qr,a1,paramj,floodAlpha,floodBeta, 
     1 			P, E, Ksat) 
       implicit none
       INTEGER xx, i
       DOUBLE PRECISION Vfp, Qx, Qr, a1, paramj, floodAlpha, floodBeta,
     1				P, E, Ksat
       DIMENSION Vfp(xx), Qx(xx), Qr(xx), a1(xx),
     1 paramj(17),P(xx),E(xx)


       DO i=2,xx
        Qr(i)=Vfp(i-1) * paramj(15)
		IF(Vfp(i-1)* floodAlpha + floodBeta<=0)THEN
			a1(i)=0
		ELSE
			a1(i)=Vfp(i-1)* floodAlpha + floodBeta
		END IF 
		Vfp(i)=Vfp(i-1)+Qx(i)-Qr(i)-(E(i)-P(i))*a1(i) -
     1     Ksat*a1(i)
       ENDDO
       END SUBROUTINE  

*******************************************************************************/
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
		double * floodplain_gw_max_infiltration
	)
{
	int debugflag;
	double Vtemp1=0,Vtemp2=0,fact=0,dQ1,dQ2,dt;
	double infiltPotential,totalStorageAvail,Qaquifer;
	double floodWaveDuration=1; // floodWaveDuration=timestep_length
	double floodplainLateralExtent;
	double depthOfFlood;
	double totalStorageAvailPlQaquifer;
	
	*ierr = 0;
	debugflag=0;

	dt = timestep_length;
	if(dt<=0) dt=1;

	// Computes overbank flow 
  	*overbank_flow = 0;
	dQ1 = Qup - overbankflow_threshold;
	dQ2 = pow(dQ1,floodGamma);
  	if(dQ1>=0)
	{
		if(dQ2<=dQ1) *overbank_flow = dQ2;
		else *overbank_flow = dQ1; // Avoid having overbankflow greater than Qup-Threshold
	}

	if(debugflag>0)
	{
		printf("Qt=%0.5f g=%0.5f Qup=%0.5f dQ1=%0.5f dQ2=%0.5f q=%0.5f\n",
				overbankflow_threshold,floodGamma,Qup,dQ1,dQ2,*overbank_flow);
	}

	// Temporary estimate of floodplain volume
	Vtemp1 =   *floodplain_volume  +  *overbank_flow * timestep_length;
  	
	// Estimate of floodplain area
	*floodplain_area = floodAlpha+floodBeta*Vtemp1;
	if(*floodplain_area<0) *floodplain_area=0;

	// Loss and gain fluxes
	*rainfall_flux = *floodplain_area * rainfall * 1e-3/dt;
	*evap_flux = *floodplain_area * evap  * 1e-3/dt;
	
	// estimate the floodplain dimensions
	floodplainLateralExtent = *floodplain_area / floodplainLength;
	
	if(*floodplain_area<1e-6)
	{
		depthOfFlood = 0.0;
	}
	else
	{
		depthOfFlood = Vtemp1 / *floodplain_area;
	}
	
	if(depthToGw>0)
	{
		totalStorageAvail = depthToGw * aquiferSpecificYield * floodplainLateralExtent * floodplainLength / timestep_length;
	}
	else
	{
		totalStorageAvail = 0.0;
	}
	
	// FILE *f = fopen("C:/Users/kim079/Documents/WIRADA/AWRAII/Rscripts/output.flood.csv", "a");
	// fprintf(f,"%0.5f,%0.5f,%0.5f,%0.5f,%0.5f \n",
		// totalStorageAvail,aquiferSpecificYield,depthToGw,floodplainLateralExtent,floodplainLength);
	// fclose(f);

	infiltPotential = floodplainKsat * floodplainLateralExtent * (depthOfFlood/surfaceLayerThickness + 1) * floodplainLength * floodWaveDuration;
	//infiltPotential = *floodplain_area * floodplainKsat;
	if(floodplainLateralExtent<1e-6)
	{
		Qaquifer = 0.0;
	}
	else
	{
		Qaquifer = aquiferKsat * aquiferSaturatedThickness * floodplainLength * floodWaveDuration * (depthOfFlood / (floodplainLateralExtent/2));
	}
	
	//*groundwater_loss = *floodplain_area * floodplainKsat;
	totalStorageAvailPlQaquifer = totalStorageAvail + Qaquifer;
	if(infiltPotential<totalStorageAvailPlQaquifer)
	{
		*groundwater_loss = infiltPotential;
	}
	else
	{
		*groundwater_loss = totalStorageAvailPlQaquifer;
		// printf("infiltPotential=%0.5f totalStorageAvailPlQaquifer=%0.5f \n",
		// infiltPotential,totalStorageAvailPlQaquifer);
	}
	
	// print to file
	// double propFloodLoss;
	// if(*floodplain_volume>0)
	// {
		// propFloodLoss = *groundwater_loss * timestep_length / *floodplain_volume;
	// }
	// else
	// {
		// propFloodLoss = 0.0;
	// }
	// FILE *f = fopen("C:/Users/kim079/Documents/WIRADA/AWRAII/Rscripts/output.flood.csv", "a");
	// fprintf(f,"%0.5f,%0.5f,%0.5f,%0.5f,%0.5f,%0.5f \n",
	// infiltPotential,totalStorageAvail,Qaquifer,propFloodLoss,*floodplain_volume,*groundwater_loss);
	// fclose(f);
	*floodplain_gw_max_change_storage = totalStorageAvail;
	*floodplain_gw_outflow = Qaquifer;
	*floodplain_gw_max_infiltration = infiltPotential;

	// printf("aquiferSaturatedThickness=%0.5f aquiferKsat=%0.5f aquiferSpecificYield=%0.5f depthToGw=%0.5f floodplainKsat=%0.5f surfaceLayerThickness=%0.5f floodplainLength=%0.5f \n",
	//	aquiferSaturatedThickness,aquiferKsat,aquiferSpecificYield,depthToGw,floodplainKsat,surfaceLayerThickness,floodplainLength);

	// Loss and gain fluxes
	Vtemp2 = Vtemp1+timestep_length*(*rainfall_flux - *evap_flux - *groundwater_loss); 	

	// Reduction of loss fluxes in case Vtemp2<0
	if(Vtemp2<0)
	{
		Vtemp2=0;
		fact = 0;
		if(*evap_flux + *groundwater_loss>0)
			fact = (timestep_length* *rainfall_flux+Vtemp1)/(
					*evap_flux + *groundwater_loss)/timestep_length;
		*evap_flux *= fact;
		*groundwater_loss*= fact;
	}

	*return_flow = Vtemp2 * return_flow_coefficient/timestep_length;

	if(*return_flow*timestep_length<Vtemp2)
		*floodplain_volume= Vtemp2 - *return_flow*timestep_length;
	else
	{
		*return_flow = Vtemp2/timestep_length;
		*floodplain_volume = 0;
	}
	
	// Final calculation for floodplain area
	*floodplain_area = floodAlpha+floodBeta* *floodplain_volume;
	if(*floodplain_area<0) *floodplain_area=0;
	
	return;
}

