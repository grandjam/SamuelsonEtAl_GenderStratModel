# IMPORTANT NOTE! Please view the README file on Github page (https://github.com/grandjam/SamuelsonEtAl_GenderStratModel) for instructions BEFORE running this code!!

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

## Set id number for this simulation run; this number will be appended to the file names for all simulation output from this run
## NOTE: id should correspond to the number appended to the end of the orgMatDat.RData file used as input to the simulation code. For example, if you want 
## to use "orgMatDat6.RData" as the input for the simulation, change id = 6.
id <- 1

# (2) Load model script into local R environment; make sure the file with model code is located in the same directory in which this simulation code is run
source("GenderStratificationModelFunction_v3_Study2.R")

# (3) Load orgMat data
## NOTE: code assumes that user has the needed orgMatDat.RData files stored in the same working directory as the model code. 
load(paste(getwd(), "/orgMatDat", id, ".RData", sep = ""))

# (4) Export all data necessary to run the model and initialize the random number generator for each core
sfExportAll()
sfClusterSetupRNGstream() # Set seed so that results can be replicated

# (5) Run simulation
startTime <- Sys.time()
condsDat <- sfClusterApplyLB(orgMatList, function(x) sim(orgMat = as.data.frame(x)))
runTime <- Sys.time() - startTime
runTime

# (6) Save data
# This code for submitting and saving multiple jobs on the HPCC
startSave <- Sys.time()
save(condsDat, file = paste("condsDatStudy2_",id,".RData",sep = ""))
saveTime <- Sys.time() - startSave
saveTime

# (7) Stop cluster and return R to running on single-core
sfStop()