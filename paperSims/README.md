# Description
This folder contains all model code, simulation scripts, and data files reported for the two simulation studies in Samuelson et al. The **GenderStratifcationModel_Study1.R** and **GenderStratificationSimulation_Study1.R** script files are identical to those described in the base model. The **GenderStratificationModel_Study2.R** and **GenderStratificationSimulation_Study2.R** script files are similar in functionality, but are slightly altered in terms of how they are initialized. Specifically, the `sim` function used to initialize and simulate a single organization in **GenderStratificationModel_Study2.R** takes an additional argument labeled `orgMat`. This argument expects a data frame containing the breakdown of male and female agents at each level of the organization.

# Running the simulations
## Study 1
The procedure for running the simulations for Study 1 are identical to those described for running the base version of the gender stratification model. See the instructions in the README file located in the base folder of this repository.
## Study 2
