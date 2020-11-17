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
```

## First steps

### R project

First, you must click on the *project.Rproj* file to launch these replication materials as an R project. This will set the working directory to the root of the folder containing the *project.Rproj* file; hence, you won't need to set the working directory.

Then, you can navigate to the different R scripts in the **Files** tab in Rstudio (usually in the lower right corner).

### Simulations

Some of the figures, analysis, and results are based on simulations. Before you run other scripts, you should start by running the simulations. **Note: This can take quite a few minutes (~ 45 minutes). It doesn't stop when the loading bar is at 100%, but later on when the process is finish. The new csv file it creates should be ~1mb**.

Once you have launched the *project.Rproj*, naviguate to *Analysis/Proposition 1/Empirically Calibrated Simulations/* and run the *EmpiricallyCalibratedSimulation.R* script. **You will need to wait until the R process is finished before running any other scripts.**

This script will draw on the *SimulationFunction.R* script (in the same directory). Note that this version of the simulation is different from the preregistered version because an error was discovered in the preregistered version. You can consult and compare the code for the preregistered version in the *SimulationFunctions_PreRegisterd_Version_With_Error.R* script.

## Structure of the replication materials

## Steps to replicate results in the order that they are presented in the paper

1. Run *Figures/Figure 1.R* to generate Figure 1
2. Run *Figures/* to generate Figure 2 (need to have run the simulations first)