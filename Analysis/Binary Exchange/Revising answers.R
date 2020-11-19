################################################################################

# This is the script for the main results about revising answers for 
# proposition 1: Binary Exchange

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

# initially_accurate is if the first judgment was correct
# switch is whether participants changed answers between the first and the
# third judgment

d <- d %>%
  mutate(
    initially_accurate = correct_1
    , switch = ifelse(response_1==response_3, "stay","switch")
    , switch12 = ifelse(response_1==response_2, "stay","switch")
  )

########################
# Analyses and outputs #
########################

# Table of the number of switches according to being initially correct
myTable <- table(d$switch, d$initially_accurate)
myTable

# % of participants initially inaccurate who revised their answer
myTable[2] / (myTable[1] + myTable[2])

# % of participants initially accurate who revised their answer
myTable[4] / (myTable[3] + myTable[4])

# Comparison of levels of switching between the 
wilcox.test(table(d$switch, d$initially_accurate))