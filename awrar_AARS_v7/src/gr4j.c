#include "header.h"

/*** GR4J Model *********************************************
* Code written by Julien Lerat, CSIRO CLW
*
	nstates = 11

	npar = 4
	par[0] : S
	par[1] : IGF
	par[2] : R
	par[3] : TB

	config[0] : see header.h

* --- Versioning ---
* v01	2012-07-29 	JL 	First version of code
*
*/

/*** utility functions (see hydro_utils.c)*******************************************/
double logbound(double v);
double invlogbound(double v);
double max(double v1,double v2);
double minmax(double min,double max,double input);
double INISTATEPDT(double *mPmE, double XS, double PERCFACTOR);
double SS1(double I,double C,double pas);
double SS2(double I,double C,double pas);


/*** Parameter utility functions **********************************************/
void gr4j_namepar(int * ierr,int * npar,double * config,char ** names)
{
	*ierr=0;
	if(*npar<4) *ierr=10100;
	if(*ierr>0) return;

	names[0] = "S"; 	// S
	names[1] = "IGF"; 	// IGF
	names[2] = "R"; 	// R
	names[3] = "TB"; // TB

	return;
}

void gr4j_defaultpar(int * ierr,int * npar,double * config,double * par)
{
	*ierr=0;
	if(*npar<4) *ierr=10100;
	if(*ierr>0) return;

	par[0] = 350; 	// S
	par[1] = -1; 	// IGF
	par[2] = 40; 	// R
	par[3] = 0.5; // TB

	return;
}

void gr4j_minmaxpar(int * ierr,int * npar,double * config,double * par)
{
	*ierr=0;
	if(*npar<4) *ierr=10101;
	if(*ierr>0) return;

	par[0] = minmax(1,1e5,par[0]); 	// S
	par[1] = minmax(-50,50,par[1]); 	// IGF
	par[2] = minmax(1,1e5,par[2]); 	// R
	par[3] = minmax(0.5,50,par[3]); // TB
	return;
}
void gr4j_trans2true(int * ierr,int * npar,double * config,double * partrans,double * partrue)
{
	int i,debugflag;

	debugflag=0;

	*ierr=0;
	if(*npar<4) *ierr=10201;
	if(*ierr>0) return;

	partrue[0] = invlogbound(partrans[0]); 	// S
	partrue[1] = sinh(partrans[1]); 	// IGF
	partrue[2] = invlogbound(partrans[2]); 	// R
	partrue[3] = invlogbound(partrans[3]);// TB
	
	gr4j_minmaxpar(ierr,npar,config,partrue);

	if(debugflag>0) printf("\t\t(from C) par trans 2 par true\n");
	if(debugflag>0) for(i=0;i<4;i++) 
		printf("\t\t(from C)\tpar[%d] : trans=%0.3f -> true=%0.3f\n",
			i,partrans[i],partrue[i]);

	return;
}
void gr4j_true2trans(int * ierr,int *npar,double * config,double * partrue,double * partrans)
{
	int i,debugflag;

	debugflag=0;

	*ierr=0;
	if(*npar<4) *ierr=10202;
	if(*ierr>0) return;

	gr4j_minmaxpar(ierr,npar,config,partrue);

	partrans[0] = logbound(partrue[0]); 	// S
	partrans[1] = asinh(partrue[1]); 	// IGF
	partrans[2] = logbound(partrue[2]); 	// R
	partrans[3] = logbound(partrue[3]);// TB

	if(debugflag>0) printf("\t\t(from C) par true 2 par trans\n");
	if(debugflag>0) for(i=0;i<4;i++) 
		printf("\t\t(from C)\tpar[%d] : true=%0.3f -> trans=%0.3f\n",
			i,partrue[i],partrans[i]);

	return;
}


/*******************************************************************************
* Initialise gr4j uh and states
*
*
*******************************************************************************/
void gr4j_initialise(int * ierr,int * nuh,
			double * config,double * par,double * uh,double * inputs,
			double * statesini,double * statesuhini)
{
	int i,nuh1,nuh2,debugflag;
	int npar,ninputs,nstates;
	double PERCFACTOR,mPmE[2],tslength;

	//idmod = (int) config[14];
	npar = (int) config[15];
	nstates = (int) config[16];
	ninputs = (int) config[17];

	debugflag=0;

	*ierr=0;
	if(npar<4) 		*ierr=10301;
	if(ninputs<2) 	*ierr=10302;
	if(nstates<3) 	*ierr=10303;

	if(npar>=NPARMAX) 			*ierr=10304;
	if(ninputs>=NINPUTSMAX) 	*ierr=10305;
	if(nstates>=NSTATESMAX) 	*ierr=10306; //10308

	if(*ierr>0) return;

	// Check parameters
	gr4j_minmaxpar(ierr,&npar,config,par);
	if(debugflag>0) printf("\t\t(from C) gr4j initialise, parameter values\n");
	if(debugflag>0) for(i=0;i<npar;i++) 
		printf("\t\t(from C)\tinitialisepar[%d]=%0.3f\n",i,par[i]);

	// Configuration
	tslength = config[0];
	if (tslength>=43200){PERCFACTOR=2.25;}		// Daily time steps
	if (tslength<43200) {PERCFACTOR=4;} 		// Hourly time steps
	mPmE[0] = config[2]; // mean annual P-PE where P-PE>=0 (mm)
	mPmE[1] = config[3]; // mean annual P-PE where PE-P>0 (mm)

	// UH dimensions
	if(tslength<=21600){*nuh=NUHMAX;} 
	if(tslength>21600){*nuh=(int)(NUHMAX/5);}
	if(tslength>=86400){*nuh=(int)(NUHMAX/20);}
	nuh1 = (int)((double)*nuh/3);
	nuh2 = (int)((double)*nuh/3)*2;

	// UH ordinates
	for(i=0;i<nuh1;i++) 
		uh[i]=SS1((double)(i+1),par[3],tslength)-SS1((double)(i),par[3],tslength);

	for(i=0;i<nuh2;i++) 
		uh[nuh1+i]=SS2((double)(i+1),par[3],tslength)-SS2((double)(i),par[3],tslength);			

	// UH states
	for(i=0;i<nuh1+nuh2;i++) statesuhini[i]=0;	

	// GR4J states
	for (i=0;i<nstates;i++) statesini[i]=0;
  	statesini[1] = par[0]*INISTATEPDT(mPmE,par[0],PERCFACTOR); // S
	statesini[2] = par[2]*0.4; // R

	if(debugflag>0) printf("\t\t(from C) gr4j initialise, states\n");
	if(debugflag>0) for(i=0;i<nstates;i++)printf("\t\t(from C)\tstates[%2d]=%0.3f\n",
			i,statesini[i]);

	if(debugflag>1) printf("\t\t(from C) gr4j initialise, UH\n");
	if(debugflag>1) for(i=0;i<*nuh;i++)printf("\t\t(from C)\tuh[%2d]=%0.3f\n",
			i,uh[i]);

	return;
}

/*******************************************************************************
* Run time step code for the GR4J rainfall-runoff model
* 
* --- Inputs
* ierr			Error message
* nconfig		Number of configuration elements (1)
* npar			Number of parameters (4)
* ninputs		Number of inputs (2)
* nstates		Number of states (1 output + 2 model states + 8 variables = 11)
* nuh			Number of uh ordinates (2 uh)
*
* config 		Configuration data. 1D Array nconfig(1)x1
*				(see header.h) 
*
* par			Model parameters. 1D Array npar(4)x1
*					par[0] = S
*					par[1] = IGF
*					par[2] = R
*					par[3] = TB
*
* uh			uh ordinates. 1D Array nuhx1
*
* inputs		Model inputs. 1D Array ninputs(2)x1
*
* -- Outputs
* states		Output and states variables. 1D Array nstates(11)x1
* uhstates		uh content. 1D Array nuhx1
*
*******************************************************************************/
void gr4j_runtimestep(int * ierr,int * nuh,
			double * config,double * par,double * uh,double * inputs,
			double * states,double * statesuh)
{
	int k,l,nuh1,nuh2,debugflag;
	int npar,ninputs,nstates;
	double Q,P,E,PERCFACTOR,tslength,area,Qobs; 
	double EN,ES,PS,PR,WS,S2,PERC,ECH,TP,R2,QR,QD;
	double ech1,ech2,weight_obs;

	//idmod = (int) config[14];
	npar = (int) config[15];
	nstates = (int) config[16];
	ninputs = (int) config[17];
	weight_obs = config[18];

	debugflag=0;

	*ierr=0;
	if(npar<4) 		*ierr=10401;
	if(ninputs<2) 	*ierr=10402;
	if(nstates<3) 	*ierr=10403;

	if(npar>=NPARMAX) 			*ierr=10404;
	if(ninputs>=NINPUTSMAX) 	*ierr=10405; 
	if(nstates>=NSTATESMAX) 	*ierr=10406;//10408

	// Case where we want to use obs but no data provided or wrong weight
	if((weight_obs<0) | (weight_obs>1)) *ierr=10407;
	if((ninputs<3) & (weight_obs>0)) *ierr=10408; // 10408

	if(*ierr>0) return;

	// Check parameters
	gr4j_minmaxpar(ierr,&npar,config,par);

	// Configuration
	tslength = max(0,config[0]);
	if (tslength>=43200){PERCFACTOR=2.25;}		// Daily time steps
	if (tslength<43200) {PERCFACTOR=4;} 		// Hourly time steps
	area = 	max(0,config[1]); // catchment area

	// UH dimensions
	nuh1 = (int)((double)*nuh/3);
	nuh2 = (int)((double)*nuh/3)*2;

	// inputs
	P = inputs[0];
	if(P<0)P=0;
	E = inputs[1];
	if(E<0)E=0;

	Qobs = -999; 
	if(ninputs>=3) Qobs = inputs[2];
	if(Qobs<0) Qobs = -999;

	if(debugflag>0) printf("\t\t(from C)\tP=%0.3f E=%0.3f Qobs=%f\n",P,E,Qobs);
	if(debugflag>0) printf("\t\t(from C) start ts ");
	if(debugflag>0) for(k=1;k<6;k++) printf("%0.1f[%d] ",states[k],k);
	if(debugflag>0) printf("\n");

	// ......Production.............
	if(P>E)	
	{
		ES=0;
		WS=(P-E)/par[0];
		if(WS>=13){WS=13;}
		PS=par[0]*(1-pow(states[1]/par[0],2))*tanh(WS)/(1+states[1]/par[0]*tanh(WS));
		PR=P-E-PS;
		EN=0;
	}
	else	
	{
		WS=(E-P)/par[0];
		if(WS>=13){WS=13;}
		ES=states[1]*(2-states[1]/par[0])*tanh(WS)/(1+(1-states[1]/par[0])*tanh(WS));
		PS=0;
		PR=0;
		EN=E-P;
	}
	states[1]+=PS-ES;

	// ......Percolation.............
	S2=states[1]/pow(1+pow(states[1]/PERCFACTOR/par[0],4),0.25);
	PERC=states[1]-S2;
	states[1]=S2;
	PR+=PERC;

	// ......UH1 ....................		
	for (k=0;k<nuh1-1;k++) statesuh[k]=statesuh[1+k]+uh[k]*PR;
	statesuh[nuh1-1]=uh[nuh1-1]*PR;
			
	// ......UH2 ....................	
	for (l=0;l<nuh2-1;l++) statesuh[nuh1+l]=statesuh[nuh1+1+l]+uh[nuh1+l]*PR;
	statesuh[(nuh1+nuh2)-1]=uh[(nuh1+nuh2)-1]*PR;

	
	// ......Potential Water exchange ....................		
	// ECH=XV(NPX+3)*(X(1)/XV(NPX+1))**3.5  // Formulation initiale
	// ECH=XV(NPX+3)*(X(1)/XV(NPX+1)-XV(NPX+5)) // Formulation N. Lemoine
	ECH=par[1]*pow(states[2]/par[2],3.5);

	// ......Routing store calculation..........		
	TP=states[2]+statesuh[0]*0.9+ECH;
	// Case where Reservoir content is not sufficient
	ech1=ECH-TP;states[2]=0; 
	if(TP>=0){states[2]=TP;ech1=ECH;}
	R2=states[2]/pow(1+pow(states[2]/par[2],4),0.25);
	QR=states[2]-R2;
	states[2]=R2;
	
	// ......Direct runoff calculation.........	
	QD=0;
	// Case where the UH cannot provide enough water
	TP=statesuh[nuh1]*0.1+ECH;
	ech2 = ECH-TP;QD=0;
	if(TP>0){QD=TP;ech2=ECH;}

	// ......TOTAL STREAMFLOW..................
	Q=QD+QR;

	/*--- RESULTS ---------------- */

	states[0]	=	Q*area/tslength*1e3; // Output in m3/s
	if((weight_obs>0) & (Qobs>=0)) states[0] = Qobs*weight_obs + states[0]*(1-weight_obs);

	if(nstates >=4) states[3]	=	Q;
	if(nstates >=5) states[4]	=	ech1+ech2;
	if(nstates >=6) states[5]	=	ES+EN;
	if(nstates >=7) states[6]	=	PR;
	if(nstates >=8) states[7]	=	QD;
	if(nstates >=9) states[8]	=	QR;
	if(nstates >=10)states[9]	=	PERC;
	if(nstates >=11)states[10]	=	statesuh[nuh1]*0.1;
	if(nstates >=12)states[11]	=	statesuh[0]*0.9;

	if(debugflag>0) printf("\t\t(from C) end ts ");
	if(debugflag>0) for(k=1;k<nstates;k++) printf("%0.1f[%d] ",states[k],k);
	if(debugflag>0) printf("\n\n");

	return;
}


// --------- Component runner --------------------------------------------------
void gr4j_runtimeseries(int * ierr, int * nval, 
	int * store_states, int *overwrite_output,	
	double * config,	
	double * partrue,
	double * inputs,
	double * states,
	double * output)
{

	int i,j,nuh[1],debugflag;
	int npar,ninputs,nstates,idmod;
	double statesini[NSTATESMAX],statesuhini[NUHMAX],uh[NUHMAX];

	*ierr=0;

	idmod = (int) config[14];
	npar = (int) config[15];
	nstates = (int) config[16];
	ninputs = (int) config[17];

	debugflag=0;

	if(debugflag>0)
	{
		printf("\t\t(from C) -- run gr4j time series --\n");
		printf("\t\t(from C)\tidmod = %d\n",idmod);
		printf("\t\t(from C)\tnpar    = %d\n",npar);
		printf("\t\t(from C)\tnstates = %d\n",nstates);
		printf("\t\t(from C)\tninputs = %d\n",ninputs);
		printf("\t\t(from C)\tstore_states = %d\n",*store_states);
		printf("\t\t(from C)\toverwrite = %d\n",*overwrite_output);
		for(i=0;i<npar;i++) 
			printf("\t\t(from C)\tpar[%2.2d] = %f\n",i,partrue[i]);
	}

	// Check inputs
	if((*overwrite_output!=0) & (*overwrite_output!=1))
	{
		*ierr = 30000101;
		return;
	}
	if((idmod!=1))
	{
		*ierr = 30000102;
		return;
	}

	// Initialisation of states
  	gr4j_initialise(ierr,nuh,
			config,partrue,uh,inputs,statesini,statesuhini);

	if(*ierr>0) return;

	if(debugflag>1) for(j=0;j<nstates;j++) 
		printf("\t\t(from C)\tstatesini[%2d]=%0.3f\n",j,statesini[j]);

	// Run timeseries
	for(i=0;i<*nval;i++)
	{
		if(debugflag>1)
		{
			for(j=0;j<ninputs;j++) 
				printf("\t\t(from C) start ts %4d : run comp inputs[%2d]=%0.3f\n",
					i,j,inputs[ninputs*i+j]);
		}

		// Run timestep model and update states
		// Depends on model
		gr4j_runtimestep(ierr,nuh,
			config,partrue,uh,&(inputs[ninputs*i]),statesini,statesuhini);

		if(*ierr>0)
		{
			printf("\t\t(from C) run_comp idmod=%d - error %d at time step %d\n",
						idmod,*ierr,i);
			for(j=0;j<30;j++) 
				printf("\t\t(from C) \t\tconfig[%d]=%0.3f\n",j,config[j]);
			for(j=0;j<ninputs;j++) 
				printf("\t\t(from C) \t\tinputs[%d,%d]=%0.3f\n",i,j,inputs[ninputs*i+j]);
			for(j=0;j<npar;j++) 
				printf("\t\t(from C) \t\tpars[%d]=%0.3f\n",j,partrue[j]);
			for(j=0;j<nstates;j++) 
				printf("\t\t(from C) \t\tstates[%d]=%0.3f\n",j,statesini[j]);
			return;
		}
	
		if(debugflag>1) for(j=0;j<nstates;j++) 
			printf("\t\t(from C) end ts %4d : run comp statesini[%2d]=%0.3f\n",
					i,j,statesini[j]);

		// Store output variable
		if(*overwrite_output==1) output[i] = statesini[0];
		else output[i] += statesini[0];

		if(debugflag>1) printf("\t\t(from C) ts %4d : run comp output=%0.3f\n\n",
					i,output[i]);

		// Store states if store_states=1)
		if(*store_states==1) 
			for(j=0;j<nstates;j++) states[nstates*i+j]=statesini[j];
	}

	return;
}

