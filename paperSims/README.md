# Description
This folder contains all model code, simulation scripts, and data files reported for the two simulation studies in Samuelson et al. The **GenderStratifcationModel_Study1.R** and **GenderStratificationSimulation_Study1.R** script files are identical to those described in the base model. The **GenderStratificationModel_Study2.R** and **GenderStratificationSimulation_Study2.R** script files are similar in functionality, but are slightly altered in terms of how they are initialized. Additonally, folders containing the final simulation data, the initialization data used for Study 2, and the R scripts used to organize and create plots from the simulation data are provided.

# Running simulations
## Study 1
The script file **GenderStratificationSimulation_Study1.R** provides the code for running the Study 1 simulations. The procedure for running this simulation is identical to that described for running the base version of the gender stratification model. See the instructions in the README file located in the base folder of this repository for more information.
## Study 2
The script file **GenderStratificationSimulation_Study2.R** provides the code for running the Study 2 simulations. Running this script generally follows the same procedures as the Study 1 simulation, but differs in the following ways:
- The values for `externalGender` and `hotJobEffSize` are set to .5 and 0 (respectively) in the `sim` function to represent an organization without any structural biases favoring males
- The `sim` function used to initialize and simulate a single organization takes an additional argument labeled `orgMat`. This argument expects a data frame in the same structure as that which is output from the base gender stratification model.

The steps below provide an overview of how to run the Study 2 simulations using the provided code:
1. Download the **GenderStratificationModel_Study2.R**, **GenderStratificationSimulation_Study2.R**, and *ALL* files from the **orgMatData** folder to the same location on your compute. The files in the orgMatData folder are labeled orgMatDat1-10.R. Each of these data files contains a list of data frames describing the final employee data from 1000 of the organizations simulated as part of Study 1. These data are used as the starting values for the simulations in Study 2.
2. The current simulation setup uses three R packages to facilitate processing speed and data organization. If not already installed on your computer, these should be installed by running the following commands in the R console window:
```
install.packages("parallel")
install.packages("snowfall")
install.packages("rlecuyer")
```
3. The simulation is coded to utilize parallel processing. There are two options for running the model in parallel. These options are controlled on lines 11-15 of the code in the GenderStratificationSimulation_Study2.R file. Note that only **one** of these options should be selected.
   - **Option #1:** Use this option to run the model in parallel on your local computer using multiple cores of your computer.  *Note that Option 1 is selected by default*
   - **Option #2:** Use this option to run the model in parallel that will be distributed across cores on multiple computers.
4. Line 20 requires a value for the variable `id` to be set. This value should be set equal to the orgMatDat.R file that you wish to use as input for the Study 2 simulation. For example, if you wish to use orgMatDat7.RData as the input file, set `id = 7` prior to running the code.
5. To run the simulation under the default settings, select and execute lines 9-47 in the GenderStratificationSimulation_Study2.R script flie. Assuming the user selected Option 1 in Step #3 above, a new object called `condsDat` will be created once the simulation is complete that contains all the raw data from the simulation. Additionally, the output data from the run will be saved to the user's current working directory.
