#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

// Max length for string
#define NMAXSTRING 200

// Maximum number of inflows to reach
#define NINFLOWMAX 10

// Maximum number of lag
#define NMAXLAG 10

// Number of parameters for the irrigation model
#define NPARIRRIG 5

// Number of inputs for the irrigation model
#define NINPUTSIRRIG 8

// Number of states for the irrigation model
#define NSTATESIRRIG 23

// Number of configuration data for the irrigation model
#define NCONFIGIRRIG 13

// Number of parameters not related to routing
#define NPARNONROUTING 15

// Number of inputs not related to routing
#define NINPUTSNONROUTING 16

// Number of states not related to routing
#define NSTATESNONROUTING 39

// Number of routing parameters
#define NPARROUTING 3

// Number of routing inputs
#define NINPUTSROUTING 1

// Number of configuration parameters
#define NCONFIG 24

// Number of routing states
#define NSTATESROUTING (4+NMAXLAG)

// Minimum value to compute a log
#define MINILOG 1e-10

// Large value used in min/max bounds
#define LARGEVALUE 1e30

// Max lengths for vectors in GR4J code
#define NINPUTSMAX 10
#define NPARMAX 100
#define NSTATESMAX 200
#define NUHMAX 600
#define NELAPSEDMAX 200
#define NCONFIGMAX 30

//define constant PI
#define PI 3.141592653589793238462