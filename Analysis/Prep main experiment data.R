################################################################################

# This script is called to prepare the empirical data for the main experiment,
# set in Data/

################################################################################

###############
# Preparation #
###############

# Cleaning environment
rm(list=ls());gc()

# Loading dependencies
source("dependencies.R")
require(tidyverse)
require(xtable)

##################################
# Prepping the experimental data #
##################################

# Reading the data
d = read.csv("Data/Crowd Classification Problem - Main Experiment Data.csv", stringsAsFactors=F) %>%
  mutate(
    correct_1 = response_1 == correctAnswer
    , correct_2 = response_2 == correctAnswer
    , correct_3 = response_3 == correctAnswer
    , is.valid = !is.na(response_1) & !is.na(response_2) & !is.na(response_3)
  ) %>% mutate(
    questionSet = NA  
  ) %>% mutate_when(
    grepl("cola", question), list(questionSet = "Pepsi vs Coke")
    , grepl("candies", question), list(questionSet = "Candies")
    , grepl("employ", question), list(questionSet = "Employment")
    , grepl("tech", question), list(questionSet = "Technology")
    , grepl("dessert", question), list(questionSet = "Calories")
  )

# Aggregate the data by trial
ag = d %>% group_by(trial_id, question, questionSet) %>%
  summarize(
    N=length(response_1)
    , correct_1 = mean(correct_1[is.valid])
    , correct_2 = mean(correct_2[is.valid])
    , correct_3 = mean(correct_3[is.valid])
    , change_13 = correct_3 - correct_1
  )

# Summarize the experimental data
empirical_sum = ag %>% 
  group_by(correct_1, change_13, questionSet, question,trial_id) %>%
  summarize(
    change_accuracy=mean(change_13)
    , initial_accuracy = mean(correct_1)
    , final_accuracy=mean(correct_3)
    #, expected_accuracy = unique(expected_accuracy)
    , N=length(correct_1)
  )
