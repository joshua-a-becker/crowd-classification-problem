################################################################################

# This is the script for the main analyses of the pilot experiment

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc();

# Loading dependencies
source("dependencies.R")
library(tidyverse)
library(ggplot2)

################
# Loading data #
################

# Pilot experiment data
d = read.csv("Data/Crowd Classification Problem - Pilot Data .csv", stringsAsFactors=F) %>%
  mutate(
    initially_correct = response_1=="More than 350"
  , switch = ifelse(response_1==response_3, "stay","switch")
  , switch12 = ifelse(response_1==response_2, "stay","switch")
  , majority_disagree = ifelse(condition<0.5, "disagree","agree")
)

############
# Analyses #
############

# Logistic regression predicting revision of answers (switching) with initial
# accuracy and whether the majority disagrees
ANALYSIS_1 = glm(switch=="switch" ~ initially_correct + majority_disagree, d, family="binomial")
ANALYSIS_1_SUM <- summary(ANALYSIS_1)

# Logistic regression predicting revision of answers (switching) with initial
# accuracy and the condition in which participants were (% of other players who agreed)
ANALYSIS_2 = glm(switch=="switch" ~ initially_correct + condition, d, family="binomial")
ANALYSIS_2_SUM <- summary(ANALYSIS_2)

###########
# Outputs #
###########

# Table of the number of switches according to being initially correct
myTable <- table(d$switch, d$initially_correct)
myTable
# % of participants initially inaccurate who revised their answer
myTable[2] / (myTable[1] + myTable[2])
# % of participants initially accurate who revised their answer
myTable[4] / (myTable[3] + myTable[4])

# Number of participants
nrow(d)

# P value of whether initial accuracy predicts likelihood of switching 
# (whilst controlling for whether the majority disagrees)
ANALYSIS_1_SUM$coefficients[11]

# P value of whether initial accuracy predicts likelihood of switching 
# (whilst controlling for the condition in which participants were 
# (% of other players who agreed))
ANALYSIS_2_SUM$coefficients[11]

# P values of whether participants were more likely to switch if the majority
# of confederates disagreed (whilst controlling for initial accuracy)
ANALYSIS_1_SUM$coefficients[12]

# Proportions of participants switching or not when majority disagrees or not
switch_majority_table <- table(d$switch, d$majority_disagree)
switch_majority_table
# % of participants who switch when majority disagrees
switch_majority_table[4] / (switch_majority_table[3] + switch_majority_table[4])
# % of participants who switch when majority agrees
switch_majority_table[2] / (switch_majority_table[1] + switch_majority_table[2])

# Participants who switch when 50% of confederates agree
d50 <- subset(d, condition == 0.5)
table(d50$switch)[2]
prop.table(table(d50$switch))[2]

# Participants who switch when 10% of confederates agree (9/10 disagree)
d10 <- subset(d, condition == 0.1)
table(d10$switch)[2]
prop.table(table(d10$switch))[2]
