################################################################################

# This is the script for the pre-registered analysis that uses OLS to test the 
# hypothesis that the magnitude of the change in accuracy (measured as the 
# absolute value) is predicted by the square of the size of the initial 
# majority (absolute value of 50 minus the initial accuracy). 

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc()

# Loading dependencies
library(tidyverse)

################
# Loading data #
################

source("Analysis/Prep main experiment data.R")

#######################
# Preparing variables #
#######################

# Creating variables for the analysis
ag = ag %>%
  mutate(
    abs_maj = abs(1-correct_1)
    , abs_maj_sq = abs_maj^2
  )

############
# Analysis #
############

ANALYSIS_5 = lm(abs(change_13) ~ abs_maj + abs_maj_sq, ag)
summary(ANALYSIS_5)