# Organization of model
The file **GenderStratificationModel.R** is the main code base for creating and simulating a single organization. Running this script creates a new function in the R environment named `sim()` that requires two arguments:
1. `externalGender`: numeric value between 0 and 1 indicating that the proportion of external agents hired into an organization that are male
2. `hotJobEffSize`: numeric value between 0 and 1 representing the difference between the center of the distributions from which male and female opportunity values were sampled

# Running simulations
The script file, **GenderStratificationSimulation.R**, provides a template and code for conducting simulations with the gender stratification model and can be used to manipulate parameters, create experimental conditions, and generate data for simulated organizations.

The steps below provide an overview of how to run simulations using the base gender stratification model:
1. Download all files in this repository to the same folder on your computer.
2. The current simulation setup uses three R packages to facilitate processing speed and data organization. If not already installed on your computer, these should be installed by running the following commands in the R console window:
```
install.packages("parallel")
install.packages("snowfall")
install.packages("rlecuyer")
```
3. The simulation is coded to utilize parallel processing. There are two options for running the model in parallel. These options are controlled on lines 9-13 of the code in the GenderStratificationSimulation.R file. Note that only **one** of these options should be selected.
   - **Option #1:** Use this option to run the model in parallel on your local computer using multiple cores of your computer.  *Note that Option 1 is selected by default*
   - **Option #2:** Use this option to run the model in parallel that will be distributed across cores on multiple computers.
4. Line 21 determines the number of organizations that will be simulated during this model. Under the provided default setup, the simulation will run 1000 participants (`nOrgs = 1000`). Note that the length of time required to simulate more organizations will depend on the speed of your computer, number of cores avaialable, available system RAM, etcs.
5. Line 24 in the GenderStratificationSimulation.R file creates a data frame called `conds` that specifies the parameters and values to be manipulated for a simulation. The provided code is set up to manipulate two organization-level parameters -- the proportion of external hires that are male (`externalGender`) and the difference in the value of developmental opportunity hires given to male versus female agents (`hotJobEffSize`). Note that manipulating different parameters will require the user to ensure that values are not given to these parameters in the associated initialization file.
6. To run the simulation under the default settings, select and execute lines 9-47 in the GenderStratificationSimulation.R script flie. Assuming the user selected Option 1 in Step #3 above, a new object called `condsDat` will be created once the simulation is complete that contains all the raw data from the simulation. Additionally, the input parameter values and output data from the run will be saved to the user's current working directory.
