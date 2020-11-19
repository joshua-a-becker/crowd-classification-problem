################################################################################

# This is the script to visualize the examples given in the discussion for 
# the Binary Exchange analyses.

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

# Creating variables for the analysis (and taking out the split group)
ag = ag %>%
  subset(correct_1!=0.5) %>%
  mutate(
    initially_accurate = correct_1>=0.5
    , changed = change_13!=0
    , improve = change_13>0
    , abs_maj = abs(1-correct_1)
    , abs_maj_sq = abs_maj^2
  )

#########################################################################
# Trial which starts with 47% accuracy and finished with 26% accuracy   #
# (53% inaccurate majority consolidated into a 74% inaccurate majority) #
#########################################################################

# Getting the example trial
example <- subset(ag, trial_id == 48)
# Initial accuracy 
example$correct_1
# Final accuracy
example$correct_3

##################
# Cola statistic #
##################

# Selecting the trials with the Pepsi vs Coke question
cola <- ag[ag$questionSet == "Pepsi vs Coke", ]

# Table of whether the initially inaccurate groups improved
table(cola$improve[!cola$initially_accurate])
