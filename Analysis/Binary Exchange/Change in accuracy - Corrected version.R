################################################################################

# This is the script for the main results about changes in accuracy for 
# proposition 1: Binary Exchange

# NOTE: This is the corrected version of the analysis where the group that 
# is split in terms of initial accuracy (50% correct) is considered 
# in a category of its own ("split"; i.e., not initially accurate). In the 
# pre-registered version of this script, this group is considered as 
# "initially accurate".

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
    initially_accurate = correct_1>0.5
    , changed = change_13!=0
    , improve = change_13>0
  )

# ag$initially_accurate is whether the 50% or more of the group members made
# an accurate judgment on their first judgment.

# ag$changed is whether the group changed accuracy (either increase or 
# decreased) between the first and third judgment.

# ag$improve is whether the group increased in accuracy between the first 
# and third judgment.

####################################################
# Outputs for the results about change in accuracy #
####################################################

### Groups who were initially inaccurate ###
# Number of groups who were initially inaccurate:
length(ag$change_13[!ag$initially_accurate])
# Average change in accuracy of groups who were initially inaccurate:
mean(ag$change_13[!ag$initially_accurate])
# p value for Wilcoxon rank test of change in accuracy due to initial inaccuracy
ANALYSIS_3b = wilcox.test(ag$change_13[!ag$initially_accurate])  
ANALYSIS_3b$p.value

### Groups who were initially accurate ###
# Number of groups who were initially accurate:
length(ag$change_13[ag$initially_accurate])
# Average change in accuracy of groups who were initially accurate:
mean(ag$change_13[ag$initially_accurate])
# p value for Wilcoxon rank test of change in accuracy due to initial accuracy
ANALYSIS_3a = wilcox.test(ag$change_13[ag$initially_accurate])
ANALYSIS_3a$p.value

### p value for Wilcoxon rank test of difference in change in accuracy between 
### both conditions (initially accurate and inaccurate)
ANALYSIS_1 = wilcox.test(change_13 ~ initially_accurate, ag)
ANALYSIS_1$p.value

 ######################################################
# Outputs for the results about improving conditional # 
# on having changed in accuracy                       #
#######################################################

# The data conditional on having changed in accuracy
ag_changed <- subset(ag, changed)

### Groups who were initially inaccurate ###
# Whether initially inaccurate groups became more accurate if they changed
# in accuracy
table(ag_changed$improve[!ag_changed$initially_accurate])
# proportion of these groups
prop.table(table(ag_changed$improve[!ag_changed$initially_accurate]))
#Average change of groups initially inaccurate 
mean(subset(ag_changed, !initially_accurate)$change_13)
# p value for proportion test of number of groups initially inaccurate that
# improved in accuracy
ANALYSIS_4b = prop.test(table(ag_changed$improve[!ag_changed$initially_accurate]))
ANALYSIS_4b$p.value

### Groups who were initially accurate ###
# Whether initially accurate groups became more accurate if they changed
# in accuracy
table(ag_changed$improve[ag_changed$initially_accurate])
# proportion of these groups
prop.table(table(ag_changed$improve[ag_changed$initially_accurate]))
#Average change of groups initially accurate that improved
mean(subset(ag_changed, improve & initially_accurate)$change_13)
#Average change of groups initially accurate that did not improved
mean(subset(ag_changed, !improve & initially_accurate)$change_13)
# p value for proportion test of number of groups initially inaccurate that
# improved in accuracy
x=n=sum(ag_changed$improve[ag_changed$initially_accurate])
ANALYSIS_4a = prop.test(x, n)
ANALYSIS_4a$p.value

### p value for proportion test of the difference in proportion of groups 
### improving in accuracy between both conditions (initially accurate and 
### inaccurate)
ANALYSIS_2 = prop.test(table(ag_changed$improve, ag_changed$initially_accurate))
ANALYSIS_2$p.value
