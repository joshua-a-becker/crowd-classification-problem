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
install.packages("readxl")
install.packages("httr")
install.packages("magrittr")
install.packages("here")
```

## First steps

### R project

First, you must click on the ```project.Rproj``` file to launch these replication materials as an R project. This will set the working directory to the root of the folder containing the ```project.Rproj``` file; hence, you won't will never need to worry about the working directory.

Then, you can navigate to the different R scripts in the **Files** tab in Rstudio (usually in the lower right corner).

### Simulations

Some of the figures, analysis, and results are based on simulations. Before you run other scripts, you should start by running the simulations.

#### Empirically calibrated simulation for the Binary Exchange

Once you have launched the ```project.Rproj```, naviguate to *Simulations/Empirically Calibrated Simulations/* and run the ```EmpiricallyCalibratedSimulation.R``` script. **You will need to wait until the R process is finished before running any other scripts. Note: This can take quite a few minutes (~ 45 minutes). It doesn't stop when the loading bar is at 100%, but later on when the process is finish. The new csv file it creates (e.g., empirical_sim_111111.csv) should be ~1mb.**

This script will draw on the ```SimulationFunction.R``` script (in the same directory). Note that this version of the simulation is different from the preregistered version because an error was discovered in the preregistered version. You can consult and compare the code for the preregistered version in the ```SimulationFunctions_PreRegisterd_Version_With_Error.R``` script.

#### Numeric simulation for the Numeric Exchange

Now, navigate to *Simulations/Numeric Simulations/* and run the ```Numeric Simulation - Run Simulations.R``` script. This will draw on the ```Numeric Simulation - Generate A Matrix.R``` script. **You will need to wait until the R process is finished before running any other scripts. Note: This can take quite a few minutes (~ 2 hours). You need to wait until the numbers printed to the console reach 1000 (printed four times). The new csv file it creates (e.g., numsim1111111.csv) should be ~13.5mb.**

## Structure of the replication materials

### Data 

This is where all the experimental data files will be. Many are accessed from the internet, the only files kept in this repository are:
- Crowd Classification Problem - Main Experiment Data.csv
- Crowd Classification Problem - Pilot Data .csv
- gurcay_data.csv 

The *lorenz_et_al.xls* file is downloaded from the internet before being access from here, but it is not kept in the repository.

#### Main experimental data for the binary exchange

The main experimental data is provided by *Data/Crowd Classification Problem - Main Experiment Data.csv*. This data is accessed and prepped by *Analysis/Prep main experiment data.R*. This script that prepares the data is sourced by the scripts that need this data. This will produce **d** with the main experimental data, **ag** with the data aggregated over trials, and **empirical_sum** with the data summarized.

#### Data for the numeric exchange

The data from the numeric exchange is accessed from these datasets:
- Gurcay et al. (accessed from */Data*)
- Becker et al. (accessed directly online)
- Lorenz et al. (downloaded from online and then accessed from */Data*)

**You do not need to worry about obtaining these datasets, the code will do it on its own.**

These datasets will be accessed and processed by ```Analysis/Load data for numeric exchange.R``` for most of the scripts which need this data. This will generate a **d** dataframe with all three datasets, and a **d1**, **d2**, and **d3** for each datasets respectively.

### Figures

This is where all the figures generated will appear (none of them are kept in the repository).

### Analysis

This is where the scripts for analysing the data and getting the results are held. They are separated into folders according to when these results appear in the paper: 
- Introduction and examples
- Binary Exchange (the first empirical analysis)
- Numeric Exchange (the second empirical analysis)
- Appendices

## Steps to replicate results in the order that they are presented in the paper

1. Run ```Analysis/Introduction and examples/Figure 1 and illustrative example.R``` to generate Figure 1
1. Run ```Analysis/Binary Exchange/Figure 2.R``` to generate Figure 2
1. For the results about changes in accuracy for Binary Exchange see ```Analysis/Binary Exchange/Change in accuracy - Corrected version.R``` and ```Analysis/Binary Exchange/Change in accuracy - Pre-registered version.R``` 
1. Run ```Analysis/Binary Exchange/Table 2.R``` to generate Table 2
1. For statistics about answer revision (switching) for the main experiment, see ```Analysis/Binary Exchange/Revising answers.R```, and for the pilot experiment see ```Analysis/Appendices/Pilot Experiment/Pilot Experiment - Pre-Registered Analyses.R```
1. For the discussion example statistics for the Binary Exchange, see ```Analysis/Binary Exchange/Discussion Examples.R```
1. For the statistics concerning the number of trials in which conditions fit proposition 2a, and for statistics concerning the accuracy of means across trials (proposition 2b), see ```Analysis/Numeric Exchange/Conditions and accuracy for 2a and 2b.R```
1. Run ```Analysis/Numeric Exchange/Figure 3.R``` to generate Figure 3 **(takes ~ 5 minutes)**
1. For the theoretical model fit across trials for all three datasets, see ```Analysis/Numeric Exchange/Theoretical model fit.R``` **(takes ~ 5 minutes)**
1. For the calories example in the Numeric Exchange discussion, see ```Analysis/Numeric Exchange/Discussion Example.R```
1. Run ```Analysis/Appendices/Figure A2.R``` to generate Figure A2
1. For the pre-registered OLS analysis,see ```Analysis/Appendices/Pre-Registered OLS analysis.R```
1. For the pre-registered tests of the effects of initial accuracy see ```Analysis/Appendices/Pilot Experiment/Pilot Experiment - Pre-Registered Analyses.R``` and ```Analysis/Binary Exchange/Change in accuracy - Pre-registered version.R``` 
1. For the goodness of fit of the different models, see ```Analysis/Appendices/Model Goodness of Fit.R```
1. For the results of the pilot experiment, see ```Analysis/Appendices/Pilot Experiment/Pilot Experiment - Pre-Registered Analyses.R```
1. Run ```Analysis/Appendices/Pilot Experiment/Pilot Experiment - Figure A3.R``` to generate Figure A3
1. Run ```Analysis/Appendices/Table A1.R``` to generate Table A1 **(takes ~ 5 minutes)**
1. Run ```Analysis/Appendices/Figure A4.R``` to generate Figure A4
1. Run ```Analysis/Appendices/Figure A5.R``` to generate Figure A5 **(takes ~ 5 minutes)**