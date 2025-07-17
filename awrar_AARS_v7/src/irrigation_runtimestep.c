#include "header.h"

/******************************************************************************
Timestep code of irrigation model

-- inputs ---
int * ierr,				Error code

// Config (11)
double timestep_length : Time step length (s)
double areaIrrig_max : max irrigation area (km2)
double licVol : irrigation license volume (ML)
double volOFSmax : maximum On-Farm-Storage volume (ML)
double volGWmax : maximum groundwater irrigation volume (ML)
double efficiency : irrigation efficiency (dimensionless)
double returnflow_coef : fraction of diversion that is returned (dimensionless)
double soilCap : (mm)
double gamma : m2
double sigma : m
double ringtankAvgDepth : m

// Parameters (5)
double alpha_irrig :	monod alpha risk function (-)
double beta_irrig : monod beta risk function (-)
double threshOFS : OFS threshold (ML)
double maxPump : max pumping capacity (ML)
double pumpAdjust : pump adjustment factor (ML)

// Inputs (7)
double reachInflow : reach inflow (ML)
double allocation : irrigation allocation
double weighted_Kc : Weighted Crop factor
double activeAreaPro : Active irrigated area percentage
double juianDay : Julian day (-)
double rainfall : rainfall (mm)
double evaporation : evap (mm)

// states (20)
double * licPro,
double * OFSpro,
double * GWpro,
double * areaActivePro_prec,
double * areaCurrent,
double * demand,
double * volOFS,
double * volOFSin,
double * areaOFS,
double * soil,
double * runoff,
double * gation,
double * diversion,
double * diversionCarryOver,
double * drainage,
double * volGWout,
double * volOFSout,
double * GWleft,
double * licUsed,
double * licWorking


-- Authors --
Julien Lerat and Ang Yang, CSIRO CLW

-- Versions --
2013-08-22 - Second version of the code

-- R code from Justin Hughes --
# # START irri function ###################################################
#
# added by JL on 23/07/2013:
# paramk[1] : alpha monod risk
# paramk[2] : beta monod risk
# paramk[3] : threshold OFS
# paramk[4] : OFS pump adjustment factor
# paramk[5] : time base for filter
#
# irrigation function for C core in AWRA-R package
# Justin Hughes
# 23/07/2013
setwd("w:/AWRA_II/AWRA_LRG/Working/Ang/IrrigationModule")
t1<-read.csv("irrigation_test_data_Ang.csv")
paramk<-c(0.4,0.5,1000000,1,1)
areaMax <- 3.466409
licVol <- 5000
volOFSmax <- 0
volGWmax <- 0
maxPump <- 0
efficiency <- 1.4
soilCap <- 100
gamma <- 700
sigma <- 20
ringtankAvgDepth <- 4.51    #m
xx <- 13878
volMax<-licVol + volOFSmax + volGWmax

Qin <- t1$Qin
E <- t1$APET_mm
P <- t1$Rain_mm
Jday <- t1$Jday
allocation <- t1$allocation
areaActivePro2 <- t1$areaActivePro
weightedKc2 <- t1$weightedKc



irri<-function(paramk, areaMax, licVol, volOFSmax, volGWmax,Qin, E, P, Jday, allocation,
               areaActivePro2, weightedKc2, maxPump,efficiency, volMax, xx, soilCap=100, gamma=700, sigma=20, ringtankAvgDepth)

{
   areaCurrent<-rep(0, xx)			#km2
   areaCurrent[1]<-areaMax/10
   #volAvailable<-(allocation[1]*licVol) + volOFSmax + volGWmax		#ML
   demand<-rep(0, xx)			#mm
   demand[1] <- (weightedKc2[1]*areaCurrent[1] * E[1]) - (P[1]*areaActivePro2[1]*areaCurrent[1])
   volOFS<-rep(0, xx)	#ML
   volOFS[1]<-volOFSmax
   volOFSin<-rep(0, xx)	#ML
   areaOFS<-rep(0, xx)	#km2
   areaOFS[1]<-1000*volOFSmax/ringtankAvgDepth
   soil<-rep(0, xx)	#mm
   soil[1]<-100
   runoff<-rep(0, xx)	#ML
   gation<-rep(0, xx)	#ML
   diversion<-rep(0, xx)	#ML
   diversionCarryOver<-rep(0, xx)	#ML
   volGWout<-rep(0, xx)	#ML
   licUsed<-rep(0, xx)	#ML
   licWorking<-rep(0, xx)	#ML
   licWorking[1]<-allocation[1]*licVol	#ML
   GWleft<-rep(0, xx)	#ML
   GWleft[1]<-volGWmax
   volOFSout<-rep(0, xx)	#ML
   licPro<-rep(0, xx)	#unitless
   licPro[1]<-(licWorking[1]-licUsed[1])/(licWorking[1]-licUsed[1] + volOFS[1] + volGWmax)
   OFSpro<-rep(0, xx)	#unitless
   OFSpro[1]<- volOFS[1]/(licWorking[1]-licUsed[1] + volOFS[1] + volGWmax)
   GWpro<-rep(0, xx)	#unitless
   GWpro[1]<-volGWmax/(licWorking[1]-licUsed[1] + volOFS[1] + volGWmax)	#unitless
   if(is.nan(licPro[1])){
      licPro[1]<-0
   }
   if(is.nan(OFSpro[1])){
      OFSpro[1]<-0
   }
   if(is.nan(GWpro[1])){
      GWpro[1]<-0
   }
   maxIrr<-gamma/(sigma*(2*pi)^0.5) # set maximum daily irrigation (mm)

   for(j in 2:length(E))
   {
   	  licPro[j] <- licPro[j-1]
	  OFSpro[j] <- OFSpro[j-1]
	  GWpro[j] <- GWpro[j-1]
	  licWorking[j] <- licWorking[j-1]

	  # input to OFS
      if(Qin[j]>=paramk[3]){
         volOFSin[j]<-((Qin[j]-paramk[3])*maxPump)/(Qin[j]-paramk[3]+paramk[4])
      } else{
         volOFSin[j]<-0
      }

      # reset crop area and proportions to use from each water source on decision days
      if(Jday[j] %in% c(80, 120, 270, 310, 365)){
         licWorking[j]<-allocation[j]*licVol
         volAvailable<-(licWorking[j] - licUsed[j-1]) + volOFS[j-1] + GWleft[j-1]
         if((licWorking[j] - licUsed[j-1])<=0&volOFS[j-1]==0&GWleft[j-1]==0){
            licPro[j]<-0
            OFSpro[j]<-0
            GWpro[j]<-0
         }else{
            licPro[j]<-(licWorking[j] - licUsed[j-1])/((licWorking[j] - licUsed[j-1]) + volOFS[j-1] + GWleft[j-1])
            OFSpro[j]<- volOFS[j-1]/((licWorking[j] - licUsed[j-1]) + volOFS[j-1] + volGWmax)
            GWpro[j]<-GWleft[j-1]/((licWorking[j] - licUsed[j-1]) + volOFS[j-1] + GWleft[j-1])
         }
		 areaCurrent[j]<-((volAvailable/volMax*paramk[1])/((volAvailable/volMax)+paramk[2]))*areaMax
      }else{
         areaCurrent[j]<-areaCurrent[j-1]
      }

      demand[j]<-(weightedKc2[j] * E[j] - P[j] * areaActivePro2[j]) *areaCurrent[j]
      if(areaActivePro2[j]==0){
         soilCheck<-soil[j-1]
      } else if(areaActivePro2[j-1]==0){
         soilCheck<-soil[j-1]-(weightedKc2[j]/areaActivePro2[j]*E[j])+P[j]
      } else{
         soilCheck<-soil[j-1]-(weightedKc2[j]/areaActivePro2[j]*E[j])+P[j]+gation[j-1]/(areaCurrent[j-1]*areaActivePro2[j-1]*efficiency)
      }

      # this bit adjusts soil storage, initiates irrigation and runoff
      if(soilCheck<0){
         soil[j]<-soilCheck
         gation[j]<-maxIrr*areaCurrent[j]*areaActivePro2[j]*efficiency
      } else if(soilCheck>=0&soilCheck<soilCap){
         soil[j]<-soilCheck
         gation[j]<-maxIrr*exp(-1*(soil[j]^2/(2*sigma^2)))*areaCurrent[j]*areaActivePro2[j]*efficiency
      }else{
         runoff[j]<-(soilCheck-soilCap)*areaCurrent[j]*areaActivePro2[j]
         soil[j]<-soilCap
         gation[j]<-0
      }

      # set runoff to 0 in non-irrigation season
      if(Jday[j] %in% seq(170,220)){
         runoff[j]<-0
      }

      # if diversion required exceeds river flow then carry over diversion requirement to next day
      diversion[j]<-licPro[j]*gation[j]+diversionCarryOver[j-1]
      if(diversion[j]>Qin[j]){
         diversion[j]<-Qin[j]
         diversionCarryOver[j]<-licPro[j]*gation[j]+diversionCarryOver[j-1]-Qin[j]
      }

      # calculate GW and OFS use
      volGWout[j]<-GWpro[j]*gation[j]
      volOFSout[j]<-OFSpro[j]*gation[j]
      OFScheck<-volOFS[j-1]-volOFSout[j] + volOFSin[j] - (E[j] - P[j])* areaOFS[j-1]
      if(OFScheck>volOFSmax){
         volOFS[j]<-volOFSmax
      } else if(OFScheck<0) {
         volOFS[j]<-0
      } else {
         volOFS[j]<-OFScheck
      }
      areaOFS[j]<-1000*volOFS[j]/ringtankAvgDepth

      # accounting for GW and surface water licences
      GWleft[j]<-max(0, GWleft[j-1]-volGWout[j])
      licUsed[j]<-min((licUsed[j-1] + diversion[j]),licWorking[j])

      #reset surface and GW licences at the start of the irrigation year
      if(Jday[j]==178){
         licUsed[j]<-0
         GWleft[j]<-volGWmax
      }

      # cat("j = ", j, "\n")
   }

   drainage<-diversion*0.1
#   list(diversion=diversion, irrigation=gation, volOFSout=volOFSout, volGWout=volGWout,
#        areaOFS=areaOFS, volOFSin=volOFSin, drainage=drainage, soil=soil,
#        diversionCarryOver=diversionCarryOver, runoff=runoff, demand=demand,
#        P=P, E=E)
   list(licPro=licPro, OFSpro=OFSpro, GWpro=GWpro, areaCurrent=areaCurrent,demand=demand,
		volOFS=volOFS, volOFSin=volOFSin, areaOFS=areaOFS, soil=soil, runoff=runoff, irrigation=gation,
		diversion=diversion, diversionCarryOver=diversionCarryOver, drainage=drainage, volGWout=volGWout,
		volOFSout=volOFSout, GWleft=GWleft, licUsed=licUsed, licWorking=licWorking)
}

test<-irri(paramk, areaMax, licVol, volOFSmax, volGWmax,Qin, E, P, Jday, allocation,
               areaActivePro2, weightedKc2, maxPump,efficiency, volMax, xx, soilCap=100, gamma=700, sigma=20, ringtankAvgDepth)

write.csv(data.frame(date=t1$Index,test), "testR.csv", row.names=FALSE)


library(compiler)
irri<-cmpfun(irri)
# END irri function ###################################################


******** Sub routing **********************************************************/
double minmax(double min,double max,double input);
double min(double v1,double v2);
double max(double v1,double v2);

/******* Main code ************************************************************/
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
		)
{
	// Dimensions
	int debugflag = 0;

	// threshold to determine it is equal to zero
	double e = 0.0000001;
	
	// Internal variables
	double volRatio, soilCheck, OFScheck;

	double volmax = licVol + volOFSmax + volGWmax;

	//convert mm to m
	rainfall = rainfall/1000;
	evap = evap/1000;

	//reset states
	double volOFSpre, areaOFSpre, GWleftpre, licUsedpre, areaCurrentpre, soilpre, gationPre, diversionCarryOverpre;
	volOFSpre = *volOFS;
	areaOFSpre = *areaOFS;
	GWleftpre = *GWleft;
	licUsedpre = *licUsed;
	areaCurrentpre = *areaCurrent;
	soilpre = *soil;
	gationPre = *gation;
	diversionCarryOverpre = *diversionCarryOver;

	*volOFS = 0;
	*areaOFS = 0;
	*GWleft = 0;
	*licUsed = 0;
	*areaCurrent = 0;
	*soil = 0;
	*gation = 0;
	*diversionCarryOver = 0;
	
	*demand = 0;
	*volOFSin = 0;
	*runoff = 0;
	*diversion = 0;
	*volGWout = 0;
	*volOFSout = 0;
	*rechargeGWir = 0;
	*deltaS = 0;
	*Infiltration = 0;

	//*licPro = 0;
	//*OFSpro = 0;
	//*GWpro = 0;
	//*drainage = 0;
	//*areaActivePro_prec = 0;
	//*licWorking = 0;

	// OFS volume
	*volOFSin = 0;
	if(reachInflow >= threshOFS)
	  *volOFSin = (reachInflow - threshOFS) * maxPump / (reachInflow - threshOFS + pumpAdjust);
	

	// Crop area determined by Monod risk function
	// ... if julian day !=80, 120, 270 or 310, 365, areaCurrent remains the same
	*areaCurrent = areaCurrentpre;
	if((julianDay == 80) || (julianDay == 120) || (julianDay == 270) || (julianDay == 310) || (julianDay == 365))
	{
		*licWorking = allocation * licVol;
		double volAvailable = *licWorking + volOFSpre + volGWmax; 		//modified by Ang on 28/10/2013 requested by Justin

		if((*licWorking - licUsedpre <= e) && (fabs(volOFSpre) < e) && (fabs(GWleftpre) < e))
		{
			*licPro = 0;
			*OFSpro = 0;
			*GWpro = 0;
		}
		else
		{
			*licPro = *licWorking / volAvailable;	//modified by Ang with request from Justin on 21/11/2013
			*OFSpro = volOFSpre / volAvailable;		// added by Ang on 24/07/2013
			*GWpro = GWleftpre / volAvailable;

			//*licPro = min_max(0, 1, *licPro);
			//*OFSpro = min_max(0, 1, *OFSpro);
			//*GWpro = min_max(0, 1, *GWpro);
		}

		// Application of
		volRatio = volAvailable / volmax;
		volRatio = minmax(0, 1, volRatio);

		*areaCurrent = areaIrrig_max * alpha_irrig * volRatio / (volRatio + beta_irrig);
	}

	// Compute crop demand
	*demand = (weighted_Kc * evap - rainfall * areaActivePro) * *areaCurrent / timestep_length;
	//*demand = min_max(0, LARGEVALUE, *demand);

	// Compute first guess of soil storage
	if(fabs(areaActivePro) < e || fabs(areaCurrentpre) < e) soilCheck = soilpre;
	else if(fabs(*areaActivePro_prec) < e)
	{
		soilCheck = soilpre - weighted_Kc / areaActivePro * evap + rainfall;
	}
	else
	{
		soilCheck = soilpre - weighted_Kc / areaActivePro * evap + rainfall + timestep_length * gationPre / (areaCurrentpre * *areaActivePro_prec * efficiency);
	}

	// Adjust soil storage
	double maxIrr = gamma / (sigma * pow((2 * PI), 0.5)); //set maximum daily irrigation (m)
	if(soilCheck < 0)
	{
		*soil = soilCheck;
		*gation = maxIrr * *areaCurrent * areaActivePro * efficiency / timestep_length;
	}
	else if(soilCheck >= 0 && soilCheck < soilCap)
	{
		*soil = soilCheck;
		*gation = maxIrr * exp(-1 * pow(*soil, 2) / (2 * pow((double)sigma, 2))) * *areaCurrent * areaActivePro * efficiency / timestep_length;
	}
	else
	{
		*soil = soilCap;
		*gation = 0;
		*Infiltration = min(kSurf * timestep_length, soilCheck - soilCap);               //***NEW***
		*deltaS = syAq * depthToGW;                                                      //***NEW***
		*rechargeGWir = min(*Infiltration, *deltaS) * *areaCurrent * areaActivePro/timestep_length;  //***NEW***
		*runoff = (soilCheck - soilCap -  min(*Infiltration, *deltaS)) * *areaCurrent * areaActivePro / timestep_length; //***NEW***
	}


	// Reset runoff (???)
	//if((julianDay==170)|(julianDay==220)) *runoff = 0;			//*************  Here is different from R code. Commented by Ang on 24/07/13       **********//
	//if(julianDay >= 170 && julianDay <= 220) *runoff = 0;		//commented by Ang with request from Justin on 21/11/2013

	// Compute final diversion
	*diversion = *licPro * *gation + diversionCarryOverpre;
	if(*diversion >= reachInflow)
	{
		*diversionCarryOver = min(maxIrr * *areaCurrent * areaActivePro * efficiency / timestep_length, *diversion-reachInflow);		//modified by Ang with requested by Justin on 10/12/2013
		*diversion = reachInflow;
	}
	*drainage = returnflow_coef * *diversion;			//****************  if returnflow_coef is always equal to 0.1, it is the same as R code.     ******************//

	// OFS volume
	*volOFSout = min(volOFSpre, *OFSpro * *gation); // change requested by Barua Jan 2015
	OFScheck = volOFSpre - *volOFSout * timestep_length + *volOFSin * timestep_length - (evap - rainfall) * areaOFSpre;
	if(OFScheck>volOFSmax)
	{
		*volOFS = volOFSmax;
		*volOFSin =max(0, (volOFSmax - volOFSpre - *volOFSout * timestep_length - max(0,(evap - rainfall)) * areaOFSpre))/timestep_length; 
	}
	else if(OFScheck<0)
	{
		*volOFS = 0;
	}
	else
	{
		*volOFS = OFScheck; // Breaks mass balance
	}
	*areaOFS= *volOFS / ringtankAvgDepth;

	// GW volume
	*volGWout = *GWpro * *gation;

	//accounting for GW and surface water licences
	*GWleft = max(0, GWleftpre - *volGWout * timestep_length);  // Breaks mass balance
	//*GWleft = min_max(0, LARGEVALUE, *GWleft);

	//if((*licUsed + *diversion) >= *licWorking) *licUsed = *licWorking;
	*licUsed = min(licUsedpre + *diversion * timestep_length, *licWorking);

	//reset surface and GW licences at the start of the irrigation year
	if(julianDay == 178)
	{
		*licUsed = 0;
		*GWleft = volGWmax;
	}

	// Loop on areaActivePro
	*areaActivePro_prec = areaActivePro;

	//********************** end of R code ***************************************//
	//********************** removed by Ang **************************************//
	//// Additional states from crop
	//*gwloss_fromcrop = 0;
	//*application_oncrop = *demand;
	//*diversion_floodharvest = 0;

	//// Climate fluxes from OFS and crop
	//*evap_fromOFS		=	*areaOFS * evap / 1000;						//ML
	//*rainfall_onOFS		=	*areaOFS * rainfall / 1000;					//ML
	//*evap_fromcrop		=	weighted_Kc / areaActivePro * evap;			//mm
	//*rainfall_oncrop	=	weighted_Kc / areaActivePro * rainfall;		//mm

	// Debug outputs
	if(debugflag>1)
	{/*
		printf("\n\t\t-> config = ");
		for(k=0;k<*nconfig;k++) printf("%0.2f ",config[k]);

		printf("\n\n");
	*/}

	return;
}
