################################################################################

# This is the script for Table 2, the table that shows the distribution of 
# groups according to initial accuracy and their change in accuracy between
# the first and last judgment.

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc()

# Loading dependencies
library(tidyverse)
library(xtable)

################
# Loading data #
################

source("Analysis/Prep main experiment data.R")

#############################################
# Creating the variables to make the tables #
#############################################

xtab=empirical_sum %>%
  mutate(
      change = "Unchanged" 
    , initially_accurate = ifelse(initial_accuracy>0.5, "Accurate","Inaccurate")
  ) %>% mutate_when(
      change_13!=0, list(change=ifelse(change_13[change_13!=0]<0, "Decreased", "Increased"))
    , empirical_sum$initial_accuracy==0.5, list(initially_accurate="Split")
  ) %>% {table(.$change, .$initially_accurate)}

##################
# View the table #
##################

print(xtab[c("Increased","Decreased","Unchanged"),])

#############################
# LaTeX output of the table #
#############################

xtable(xtab[c("Increased","Decreased","Unchanged"),])
