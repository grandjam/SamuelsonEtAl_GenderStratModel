# ~~~~~~~~~~~~~~ #
# Run Simulation #
# ~~~~~~~~~~~~~~ #
library(parallel)
library(snowfall)
library(rlecuyer)

# (1) Initialize numer of parallel processors to use
## Option 1: For running on local computer with multiple cores. Warning: This will use the maximum number of cores available on the computer if possible!
sfInit(parallel = T, cpus = min(nrow(conds), detectCores()), type = "SOCK")
## Option 2: For running on cores distributed across multiple computers/nodes
# sfSetMaxCPUs(number = 1000)
# sfInit(parallel = T, cpus = 250, type = "MPI")
id = 1 # Set a unique id for this simulation run; this number will be appended to the file names for all simulation output from this run

# (2) Load model script into local R environment; make sure the file with model code is located in the same directory in which this simulation code is run
source("GenderStratificationModelFunction_v3_Study1.R")

# (3) Create experimental conditions to run
## Set total number of organizations to simulate for a single run; default is set to simulate 1000 organizations
nOrgs <- 1000
## conds creates a data frame containing as many rows as there are organizations to be simulated. Each row of conds contains the parameter settings that will be used for a single organization.
### The default settings implements a Monte Carlo sampling approach in which manipulated the parameter values are randomly sampled from uniform distributions
conds <- data.frame(
  externalGender = runif(nOrgs, .2, .8), 
  hotJobEffSize = runif(nOrgs, 0, 1)
)
condsList <- split(conds, seq(nrow(conds))) # conds turned into a list format to allow load balanced parallel processing to be used when running simulations

# (4) Export all data necessary to run the model and initialize the random number generator for each core
sfExportAll()
sfClusterSetupRNGstream() # Set seed so that results can be replicated

# (5) Run simulation
startTime <- Sys.time()
condsDat <- sfClusterApplyLB(condsList, function(x) sim(as.numeric(x[1]),
                                                        as.numeric(x[2])
))
runTime <- Sys.time() - startTime
runTime

# (6) Save input parameter values (conds) and final simulation data (condsDat)
save(conds, file = paste("conds",id,".RData",sep = ""))
save(condsDat, file = paste("condsDat",id,".RData",sep = ""))

# (7) Stop cluster and return R to running on single-core
sfStop()