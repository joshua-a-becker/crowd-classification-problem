# Replication materials for:  The Crowd Classification Problem
Joshua Becker (Kellogg School of Management, Northwestern Institute on Complex Systems, Northwestern University) 

Douglas Guilbeault (Annenberg School of Communication, University of Pennsylvania) 

Ned Smith (Kellogg School of Management, Northwestern Institute on Complex Systems, Northwestern University) 

# Contents

This file contains experimental datasets, code for replication analysis, and code for simulation models.

# Using these materials

## R dependencies

You need **RStudio** for the best experience when using these replication materials. 

These are the following **packages** you need to have installed (here with the code to install them):
```
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("igraph")
install.packages("expm")
install.packages("xtable")
```

## First steps

### R project

First, you must click on the ```project.Rproj``` file to launch these replication materials as an R project. This will set the working directory to the root of the folder containing the ```project.Rproj``` file; hence, you won't need to set the working directory.

Then, you can navigate to the different R scripts in the **Files** tab in Rstudio (usually in the lower right corner).

### Simulations

Some of the figures, analysis, and results are based on simulations. Before you run other scripts, you should start by running the simulations.

#### Empirically calibrated simulation for proposition 1

Once you have launched the ```project.Rproj```, naviguate to *Simulations/Empirically Calibrated Simulations/* and run the ```EmpiricallyCalibratedSimulation.R``` script. **You will need to wait until the R process is finished before running any other scripts. Note: This can take quite a few minutes (~ 45 minutes). It doesn't stop when the loading bar is at 100%, but later on when the process is finish. The new csv file it creates should be ~1mb.**

This script will draw on the ```SimulationFunction.R``` script (in the same directory). Note that this version of the simulation is different from the preregistered version because an error was discovered in the preregistered version. You can consult and compare the code for the preregistered version in the ```SimulationFunctions_PreRegisterd_Version_With_Error.R``` script.

#### Numeric simulation for proposition 2

Now, navigate to *Simulations/Numeric Simulations/* and run the ```Numeric Simulation - Run Simulations.R``` script. This will draw on the ```Numeric Simulation - Generate A Matrix.R``` script. **You will need to wait until the R process is finished before running any other scripts. Note: This can take quite a few minutes (~ 2 hours). You need to wait until the numbers printed to the console reach 1000 (printed four times). The new csv file it creates should be ~13.5mb.**

## Structure of the replication materials

### Data 

This is where all the experimental data files will be. Many are accessed from the internet, the only files kept in this repository are:
- Crowd Classification Problem - Main Experiment Data.csv
- Crowd Classification Problem - Pilot Data .csv
- gurcay_data.csv 

#### Main experimental data

The main experimental data is provided by *Data/Crowd Classification Problem - Main Experiment Data.csv*. This data is accessed and prepped by *Analysis/Prep main experiment data.R*. This script that prepares the data is sourced by the scripts that need this data. This will produce **d** with the main experimental data, **ag** with the data aggregated over trials, and **empirical_sum** with the data summarized.

### Figures

This is where all the figures generated will appear (none of them are kept on the github).

## Steps to replicate results in the order that they are presented in the paper

1. Run ```Analysis/Introduction and examples/Figure 1 and illustrative example.R``` to generate Figure 1
1. Run ```Analysis/Proposition 1/Figure 2.R``` to generate Figure 2
1. Run ```Analysis/Proposition 1/Change in accuracy - Corrected version``` and ```Analysis/Proposition 1/Change in accuracy - Pre-registered version``` for the results about changes in accuracy for proposition 1
1. Run ```Analysis/Proposition 1/Table 2.R``` to generate Table 2
1. To see statistics about answer revision (switching) for the main experiment see ```Analysis/Proposition 1/Revising answers.R```, and for the pilot experiment see XXXXXX
1. For the discussion example statistics for the Binary Exchange, see ```Analysis/Proposition 1/Discussion Examples.R```