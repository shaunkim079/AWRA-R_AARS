This is the AWRA-R model that includes Alluvium As River Storage (AARS) and dead storage (DS). The dynamic maximum version of AARS is called DMAARS.
The R package is called "awrar_AARS_v7". For an example script that loads in the library and runs the model using test data, please refer to the file: AWRA-R_AARS\awrar_AARS_v7\R\AARS_v7_example_reach_run.r
The above script also provides some basic demonstration of accessing the different model outputs.

To run the awrar.run model you need:
- a parameter vector
- a configuration info vector
- a time series data frame
- indication of using the dynamic maximum version or not i.e. useDynMax=1 or useDynMax=0
- indication of using dead storage or not i.e. useDeadStorage=1 or useDeadStorage=0

Example parameters and names are given in "AWRA-R_AARS\awrar_AARS_v7\data\test_parameters_416047.csv"
Example configuration info and names are given in "AWRA-R_AARS\awrar_AARS_v7\data\test_config_416047.csv"
Example time series input and names are given in "AWRA-R_AARS\awrar_AARS_v7\data\test_input_416047.csv"

Important notes:
- AWRA-R_AARS\awrar_AARS_v7 contains the simulation engine code (C and R) and DLL
- the main R script is "AWRA-R_AARS\awrar_AARS_v7\R\awrar.run.r"
- the C files are in "AWRA-R_AARS\awrar_AARS_v7\src"
- the DLL for Windows is "AWRA-R_AARS\awrar_AARS_v7\src\awrar_AARS_v6.dll"
- the so file for Linux is "AWRA-R_AARS\awrar_AARS_v7\src\awrar_AARS_v6.so"
- the AARS model code is in "AWRA-R_AARS\awrar_AARS_v7\src\awrar_runtimestep.c"
- "AWRA-R_AARS\awrar_AARS_v7\src\awrar_runtimestep.c" is called each time step by "AWRA-R_AARS\awrar_AARS_v7\src\awrar_run.c"
