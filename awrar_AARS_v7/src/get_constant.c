#include "header.h"

/******************************************************************************
Get integer constant defined in the header file (see header.h)

-- inputs ---
int * ierr,				Error code
char * constant_name, 	Contant name

-- outputs ---
int * value			Constant value

-- Authors --
Julien Lerat, CSIRO CLW

-- Versions --
2012-NMAXSTRING-23 - First version of the code

*******************************************************************************/
void get_constant(int * ierr, int * nstr, char ** constant_name,int * value)
{
	int debugflag;
	
	char ninflowmax[NMAXSTRING] 		= "NINFLOWMAX";
	char nparnonrouting[NMAXSTRING] 	= "NPARNONROUTING";
	char ninputsnonrouting[NMAXSTRING] 	= "NINPUTSNONROUTING";
	char nstatesnonrouting[NMAXSTRING] 	= "NSTATESNONROUTING";
	char nmaxstring[NMAXSTRING] 		= "NMAXSTRING";
	char nparrouting[NMAXSTRING] 		= "NPARROUTING";
	char ninputsrouting[NMAXSTRING] 	= "NINPUTSROUTING";
	char nstatesrouting[NMAXSTRING] 	= "NSTATESROUTING";
	char nconfig[NMAXSTRING] 			= "NCONFIG";
	char nparirrig[NMAXSTRING] 			= "NPARIRRIG";
	char ninputsirrig[NMAXSTRING] 		= "NINPUTSIRRIG";
	char nstatesirrig[NMAXSTRING] 		= "NSTATESIRRIG";
	char nconfigirrig[NMAXSTRING] 		= "NCONFIGIRRIG";
	char ninputsmax[NMAXSTRING] 		= "NINPUTSMAX";
	char nparmax[NMAXSTRING] 			= "NPARMAX";
	char nstatesmax[NMAXSTRING] 		= "NSTATESMAX"; 		
	char nuhmax[NMAXSTRING] 			= "NUHMAX";
	char nelapsedmax[NMAXSTRING] 		= "NELAPSEDMAX";
	char nconfigmax[NMAXSTRING] 		= "NCONFIGMAX";

	*ierr = 0;
	*value = 0;

	debugflag = 0;

	if(debugflag>0)
		printf("\n\nConstant name (%d): %s\n",*nstr,*constant_name);
	
	if(strcmp(*constant_name,ninflowmax)==0) 		*value=NINFLOWMAX;
	if(strcmp(*constant_name,nparnonrouting)==0) 	*value=NPARNONROUTING;
	if(strcmp(*constant_name,ninputsnonrouting)==0) *value=NINPUTSNONROUTING;
	if(strcmp(*constant_name,nstatesnonrouting)==0) *value=NSTATESNONROUTING;
	if(strcmp(*constant_name,nmaxstring)==0) 		*value=NMAXSTRING;
	if(strcmp(*constant_name,nparrouting)==0) 		*value=NPARROUTING;
	if(strcmp(*constant_name,ninputsrouting)==0) 	*value=NINPUTSROUTING;
	if(strcmp(*constant_name,nstatesrouting)==0) 	*value=NSTATESROUTING;
	if(strcmp(*constant_name,nconfig)==0) 			*value=NCONFIG;
	if(strcmp(*constant_name,nparirrig)==0) 		*value=NPARIRRIG;
	if(strcmp(*constant_name,ninputsirrig)==0) 		*value=NINPUTSIRRIG;
	if(strcmp(*constant_name,nstatesirrig)==0) 		*value=NSTATESIRRIG;
	if(strcmp(*constant_name,nconfigirrig)==0) 		*value=NCONFIGIRRIG;
	if(strcmp(*constant_name,ninputsmax)==0) 		*value=NINPUTSMAX;
	if(strcmp(*constant_name,nparmax)==0) 			*value=NPARMAX;
	if(strcmp(*constant_name,nstatesmax)==0) 		*value=NSTATESMAX;
	if(strcmp(*constant_name,nuhmax)==0) 			*value=NUHMAX;
	if(strcmp(*constant_name,nelapsedmax)==0) 		*value=NELAPSEDMAX;
	if(strcmp(*constant_name,nconfigmax)==0) 		*value=NCONFIGMAX;

	if(debugflag>0)
		printf("\nvalue = %d\n",*value);

	return;
}
