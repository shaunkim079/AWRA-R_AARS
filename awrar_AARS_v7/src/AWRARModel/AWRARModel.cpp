// AWRA-RModel.cpp : Defines the exported functions for the DLL application.
//

#include "header.h"
#include "awrar_runtimestep.h" 

extern "C"
{
__declspec(dllexport) void runtimestep(int * ierr,
		int * nconfig, 
		int * ninputs,
		int * npar,
    	int * nstates,
		double * config,
		double * inputs,
		double * parameters,
		double * states){
			awrar_runtimestep ( ierr, nconfig, ninputs, npar, nstates, config, inputs, parameters, states );
		}
}
