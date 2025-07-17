#include "header.h"
/*
 utility functions
*/

/*** Absolute value of a double *************************************/
double dabs(double x)
{
	double ret=x;
	if(x<0) ret=-x;
	return ret;
}

/*** minmax function ******************************************/
double min(double v1,double v2){
	double out=v1;
	if(v1>v2) out = v2;
	return out;
}
double max(double v1,double v2){
	double out=v1;
	if(v1<v2) out = v2;
	return out;
}
double minmax(double min,double max,double input){
	double out=input;
	if(out>max) out = max;
	if(out<min) out = min;
	return out;

}


/**** Monod function ******************************************/
double monod(double p1,double p2,double input){ 
	double out;
	
	p1 = max(0,p1);
	p2 = max(0,p2);
	
	// Function returns p1 if p2=0
	// Works even if input=0
	out = p1;
	// otherwise returns the normal Monod output
	if(p2>0) out = p1*input/(p2+input);
	
	return out;
}


/*** log bounded function ******************************************/
double logbound(double v){
	return log(dabs(v)+MINILOG);
}
double invlogbound(double v){
	return exp(v)-MINILOG;
}

/*** atanh bounded function ******************************************/
double atanhbound(double v)
{
	double v2;
	if(v<0) v=0;
	if(v>1) v=1;
	v2 = 2*(1-1e5*MINILOG)*v-1+1e5*MINILOG;
	return 0.5*log((1+v2)/(1-v2));
}
double invatanhbound(double v)
{
	double v2;
	v2 = (tanh(v)+1-1e5*MINILOG)/2/(1-1e5*MINILOG);
	if(v2<0) v2=0;
	if(v2>1) v2=1;
	return v2;
}


/*** Compute statistics from GR4J inputs to initialise the S state ****/
void compute_mPmE(int *_nval,double *inputs, double* mPmE){
	int i,nP,nE;
	double P,PE;
	mPmE[0] = 0;
	mPmE[1] = 0;

	nP=0;nE=0;
	for (i=0;i<_nval[0];i++){

		P=inputs[i];
		if(P<0) P=0;

		PE=inputs[i+_nval[0]];
		if(PE<0) PE=0;

		if(P>=PE){
			mPmE[0]=mPmE[0]+P-PE;
			nP++;
		}
		else{
			mPmE[1]=mPmE[1]+PE-P;
			nE++;
		}
	}	

	if(nP>0) mPmE[0]=mPmE[0]/(double)nP;
	else mPmE[0]=0;
	if(nE>0) mPmE[1]=mPmE[1]/(double)nE;	
	else mPmE[1]=0;

	return;
}

/*** Function to compute double triangle UH************************************/
double SSUH(double LAG,double VAL){
	double SSUH=0;
	if((VAL>0) & (VAL<=LAG)) SSUH=pow(VAL/LAG,2)/2;
	if((VAL>LAG) & (VAL<=2*LAG)) SSUH=0.5+(3*LAG-VAL)*(VAL-LAG)/2/pow(LAG,2);
	if(VAL>2*LAG) SSUH=1;
	return SSUH;
}

/*** Function to compute double triangle UH************************************/
double SSLAG(double LAG,double VAL){
	double SSUH=minmax(0,1,VAL-LAG);
	return SSUH;
}

/* ************** GR4J UH subroutines ***************************** 
*	NH  = max length of the UH
*	C   = Time constant of UH
*/
double SS1(double I,double C,double pas)
{
	double Expos;

	// Exposant de l'UH
	Expos=2.5;
	if(pas<6){Expos=1.25;}

 	if (I<0) {return 0;}
	if (I<C) {return pow(I/C,Expos);}
	if (I>=C){return 1;}
	return 0;
}

double SS2(double I,double C,double pas)
{
	double Expos;
	
	// Exposant de l'UHPARTRANSMIN
	Expos=2.5;
	if(pas<6){Expos=1.25;}
	
	if (I<0)   {return 0;}
	if (I<=C)  {return 0.5*pow(I/C,Expos);}
	if (I<2*C) {return 1-0.5*pow(2-I/C,Expos);}
	if (I>=2*C){return 1;}
	return 0;
}


/****** Subroutines to find the initial state of the production store *********/
/* fINIPDT(double Pm,double Em, double XS, double Sini, double PERCFACTOR)
*   Calculates the optimal storage level of the production store
*	  Pm = mean {rainfall-PE} when rainfall>PE  x Probability of rainfall>PE 

*	  Em = mean {PE-rainfall} when PE>rainfall x Probability of PE > rainfall

*	  XS = SMA store capacity (mm)
*	  Sini = initial filling level of the SMA store
*	  PERCFACTOR = Percolation factor
*/
double fINIPDT(double Pm,double Em, double XS, double Sini, double PERCFACTOR){
  double f,ini;
  ini=Sini;
  if (ini>1){ini=1;}
  if(ini<0){ini=0;}
  
  // Equation provided by Le Moine (2008, page 212)
  f = (1-pow(ini,2))*Pm-ini*(2-ini)*Em
        -XS*ini*(1-pow(1+pow(ini/PERCFACTOR,4),-0.25));
  return f;  
}

/* INISTATEPDT(int nval,double *mPmE, double XS, double PERCFACTOR)
*   Calculates the optimal storage level of the production store by bisection

*	  Pm = mean {rainfall-PE} when rainfall>PE
*	  Em = mean {PE-rainfall} when PE>rainfall
*	  XS = SMA store capacity (mm)

*	  PERCFACTOR = Percolation factor
*/
double INISTATEPDT(double *mPmE, double XS, double PERCFACTOR){
    int i;
    double start,end;
    
    // Iteration to find the solution of the equation provided by Le Moine 
    start=0;end=1;i=0;
    while(end-start>1e-3 && i<50){
      if(fINIPDT(mPmE[0],mPmE[1],XS,start+(end-start)/2,PERCFACTOR)>0)
        start+=(end-start)/2;
      else end-=(end-start)/2;
      i++;      
    }
    return start;   
}


/* ------- Timing functions ------------*/
void elapsed_from_start(int * nelapsed, clock_t start, double * elapsed_seconds)
{
	clock_t end;

	if(*nelapsed>=NELAPSEDMAX) *nelapsed = NELAPSEDMAX-1;
	end = clock();
	elapsed_seconds[*nelapsed] = ((double) (end - start)) / CLOCKS_PER_SEC;

	*nelapsed = *nelapsed + 1;

	return;
}

